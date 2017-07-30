{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Monad       (mapM)
import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as A
import qualified Data.Foldable       as F
import qualified Data.HashMap.Lazy   as HM
import           Data.List.NonEmpty  (NonEmpty(..), nonEmpty)
import           Data.String         (IsString(..))
import qualified Data.Text           as T
import qualified Data.Yaml           as Y
import           System.FilePath     (FilePath)


-- | The parsed configuration file.
data Config = Config
  { journalPath  :: Maybe FilePath
  , defcommodity :: T.Text
  , http         :: HTTPConfig
  , tree         :: TreeConfig
  , accounts     :: [(T.Text, AccountConfig)]
  , commodities  :: [(T.Text, CommodityConfig)]
  } deriving Show

instance Y.FromJSON Config where
  parseJSON (Y.Object o) = Config
    <$> o Y..:? "journal_file"
    <*> o Y..:  "default_commodity"
    <*> (Y.parseJSON =<<  o Y..:  "http")
    <*> (Y.parseJSON =<< (o Y..:? "tree" Y..!= Y.Null))
    <*> fmap HM.toList (Y.parseJSON =<< (o Y..:? "accounts"    Y..!= Y.Null))
    <*> fmap HM.toList (Y.parseJSON =<< (o Y..:? "commodities" Y..!= Y.Null))
  parseJSON x = A.typeMismatch "config" x

-- | Configuration for the HTTP server
data HTTPConfig = HTTPConfig
  { port      :: Int
  , staticDir :: FilePath
  } deriving Show

instance Y.FromJSON HTTPConfig where
  parseJSON (Y.Object o) = HTTPConfig
    <$> o Y..: "port"
    <*> o Y..: "static_dir"
  parseJSON x = A.typeMismatch "http" x

-- | Configuration for the account tree.
data TreeConfig = TreeConfig
  { assetAccounts     :: NonEmpty Pattern
  , equityAccounts    :: NonEmpty Pattern
  , expenseAccounts   :: NonEmpty Pattern
  , incomeAccounts    :: NonEmpty Pattern
  , liabilityAccounts :: NonEmpty Pattern
  , budgetAccounts    :: NonEmpty Pattern
  } deriving Show

instance Y.FromJSON TreeConfig where
  parseJSON = \case
      Y.Object o -> TreeConfig
        <$> (atLeastOne =<< o Y..:? "assets")      Y..!= defAssets
        <*> (atLeastOne =<< o Y..:? "equity")      Y..!= defEquity
        <*> (atLeastOne =<< o Y..:? "expenses")    Y..!= defExpenses
        <*> (atLeastOne =<< o Y..:? "income")      Y..!= defIncome
        <*> (atLeastOne =<< o Y..:? "liabilities") Y..!= defLiabilities
        <*> (atLeastOne =<< o Y..:? "budget")      Y..!= defBudget
      Y.Null -> pure $
        TreeConfig defAssets defEquity defExpenses defIncome defLiabilities defBudget
      x -> A.typeMismatch "tree" x
    where
      atLeastOne (Just (Y.String pat)) = pure . Just $ Pattern pat:|[]
      atLeastOne (Just (Y.Array  arr)) = do
        pats <- mapM Y.parseJSON (F.toList arr)
        maybe (A.typeMismatch "pattern_list" Y.Null) (pure . Just) (nonEmpty pats)
      atLeastOne (Just x) = A.typeMismatch "pattern" x
      atLeastOne Nothing = pure Nothing

      defAssets      = "assets:*":|[]
      defEquity      = "equity:*":|[]
      defExpenses    = "expenses:*":|[]
      defIncome      = "income:*":|[]
      defLiabilities = "liabilities:*":|[]
      defBudget      = "budget:*":|[]

-- | An account name pattern.
newtype Pattern = Pattern T.Text
  deriving Show

instance Y.FromJSON Pattern where
  parseJSON (Y.String s) = pure (Pattern s)
  parseJSON x = A.typeMismatch "pattern" x

instance IsString Pattern where
  fromString = Pattern . T.pack

-- | Configuration for an account
data AccountConfig = AccountConfig
  { aName     :: Maybe T.Text
  , aURL      :: Maybe T.Text
  , aCategory :: Maybe T.Text
  } deriving Show

instance Y.FromJSON AccountConfig where
  parseJSON (Y.Object o) = AccountConfig
    <$> o Y..:? "name"
    <*> o Y..:? "url"
    <*> o Y..:? "category"
  parseJSON (Y.String s) = pure $
    AccountConfig (Just s) Nothing Nothing
  parseJSON Y.Null = pure $
    AccountConfig Nothing Nothing Nothing
  parseJSON x = A.typeMismatch "account" x

-- | Configuration for a commodity
data CommodityConfig = CommodityConfig
  { cName       :: Maybe T.Text
  , cURL        :: Maybe T.Text
  , cAllocation :: NonEmpty (T.Text, Int)
  } deriving Show

instance Y.FromJSON CommodityConfig where
  parseJSON = \case
      Y.Object o -> CommodityConfig
        <$> o Y..:? "name"
        <*> o Y..:? "url"
        <*> (allocation =<< o Y..:? "allocation")
      Y.String s -> pure $
        CommodityConfig (Just s) Nothing defAllocation
      Y.Null -> pure $
        CommodityConfig Nothing Nothing defAllocation
      x -> A.typeMismatch "commodity" x
    where
      allocation (Just (Y.Object o)) = do
        bits <- mapM (\(s,n) -> (,) s <$> Y.parseJSON n) (HM.toList o)
        maybe (A.typeMismatch "allocation_list" Y.Null) pure (nonEmpty bits)
      allocation (Just (Y.String s)) = pure ((s, 1):|[])
      allocation (Just Y.Null) = pure defAllocation
      allocation (Just x) = A.typeMismatch "allocation" x
      allocation Nothing = pure defAllocation

      defAllocation = ("Cash", 1):|[]
