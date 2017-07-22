{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Applicative ((*>), (<|>))
import           Control.Monad       (mapM)
import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as A
import           Data.Foldable       (toList)
import qualified Data.HashMap.Lazy   as HM
import qualified Data.Text           as T
import qualified Data.Yaml           as Y
import           System.FilePath     (FilePath)


-- | The parsed configuration file.
data Config = Config
  { port :: Int
  , staticdir :: FilePath
  , assetAccounts :: [Account]
  , liabilityAccounts :: [Account]
  , incomeRules :: AccountRules
  , budgetRules :: AccountRules
  , expenseRules :: AccountRules
  , equityRules :: AccountRules
  } deriving Show

instance Y.FromJSON Config where
  parseJSON (Y.Object o) = o Y..: "http" >>= \case
    Y.Object httpcfg -> Config
      <$> httpcfg Y..: "port"
      <*> httpcfg Y..: "static_dir"
      <*> (A.parseJSON =<< o Y..: "assets")
      <*> (A.parseJSON =<< o Y..: "liabilities")
      <*> (A.parseJSON =<< o Y..: "income")
      <*> (A.parseJSON =<< o Y..: "budget")
      <*> (A.parseJSON =<< o Y..: "expenses")
      <*> (A.parseJSON =<< o Y..: "equity")
    x -> A.typeMismatch "http" x
  parseJSON x = A.typeMismatch "config" x

-- | Rules for an account.
data Account = Account
  { accName :: T.Text
  , accBreakdown :: [Subaccount]
  } deriving Show

instance Y.FromJSON Account where
  parseJSON (Y.Object o) = Account
    <$> o Y..: "name"
    <*> (Y.parseJSON =<< o Y..: "breakdown")
  parseJSON x = A.typeMismatch "account" x

-- | Rules for a subaccount.
data Subaccount = Subaccount
  { subName :: Maybe T.Text
  , subHledgerAccount :: T.Text
  , subTag :: [(T.Text, Int)]
  , subBalTag :: Maybe T.Text
  , subURL :: Maybe T.Text
  } deriving Show

instance Y.FromJSON Subaccount where
  parseJSON (Y.Object o) = do
    tag <- o Y..:? "tag" >>= \case
      Just (Y.String tags) -> pure [(tags, 100)]
      Just (Y.Object tago) -> HM.toList <$> mapM Y.parseJSON tago
      Just x  -> A.typeMismatch "tag" x
      Nothing -> pure [("Cash", 100)]
    Subaccount
      <$> o Y..:? "name"
      <*> o Y..:  "account"
      <*> pure tag
      <*> o Y..:? "balance_tag"
      <*> o Y..:? "url"
  parseJSON x = A.typeMismatch "subaccount" x

-- | Rules for an account name.
data AccountRules
  = Only [(T.Text, T.Text)]
  -- ^ A fixed list of transformations.
  | Try [AccountRules]
  -- ^ Try the rules in order.
  | Simple T.Text
  -- ^ Use the simple rule (drop prefix + capitalise words)
  | None
  -- ^ Forbid everything.
  deriving Show

instance Y.FromJSON AccountRules where
  parseJSON (Y.Array  v) = Try . toList <$> mapM A.parseJSON v
  parseJSON (Y.Object o) = only <|> simple <|> none where
    only = o Y..: "only" >>= \case
      Y.Object o_ -> Only . HM.toList <$> mapM A.parseJSON o_
      x -> A.typeMismatch "only" x
    simple = o Y..: "simple" >>= \case
      Y.String s_ -> pure (Simple s_)
      x -> A.typeMismatch "simple" x
    none = (o Y..: "none" :: A.Parser A.Value) *> pure None
  parseJSON Y.Null = pure None
  parseJSON x = A.typeMismatch "account rules" x

-- | Rules for an account description.
data AccountDescription = AccountDescription
  { accLongName :: Maybe T.Text
  , accTag :: T.Text
  , accURL :: Maybe T.Text
  } deriving Show

instance Y.FromJSON AccountDescription where
  parseJSON (Y.Object o) = AccountDescription
    <$> o Y..:? "name"
    <*> o Y..:  "tag"
    <*> o Y..:? "url"
  parseJSON x = A.typeMismatch "account description" x
