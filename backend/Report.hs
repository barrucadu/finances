{-# LANGUAGE OverloadedStrings #-}

module Report where

import qualified Data.Aeson         as A
import           Data.List.NonEmpty (toList)
import           Data.Maybe         (catMaybes)
import           Data.Ratio         (Rational)
import qualified Data.Text          as T
import qualified Data.Time.Calendar as C
import qualified Data.Time.Format   as C

import qualified Config as FC

-- | A report to send to the user.
data Report = Report
  { rpWhen        :: C.Day
  , rpAssets      :: [AccountReport]
  , rpLiabilities :: [AccountReport]
  , rpIncome      :: BasicReport
  , rpBudget      :: BasicReport
  , rpExpenses    :: BasicReport
  , rpEquity      :: [(T.Text, Rational)]
  , rpHistory     :: DatedTransactionsReport
  , rpCommodities :: [(T.Text, CommodityReport)]
  } deriving Show

instance A.ToJSON Report where
  toJSON rp = A.object
    [ "when"        A..= T.pack (C.formatTime C.defaultTimeLocale "%B %_Y" (rpWhen rp))
    , "date"        A..= T.pack (C.formatTime C.defaultTimeLocale "%F" (rpWhen rp))
    , "assets"      A..= rpAssets      rp
    , "liabilities" A..= rpLiabilities rp
    , "income"      A..= rpIncome      rp
    , "budget"      A..= rpBudget      rp
    , "expenses"    A..= rpExpenses    rp
    , "equity"      A..= A.object [ name A..= toDouble bal | (name, bal) <- rpEquity rp ]
    , "history"     A..= rpHistory rp
    , "commodities" A..= A.object [ name A..= cr | (name, cr) <- rpCommodities rp ]
    ]

-- | A summary of an account.
data AccountReport = AccountReport
  { arName      :: T.Text
  , arBreakdown :: [SubaccountReport]
  } deriving Show

instance A.ToJSON AccountReport where
  toJSON ar = A.object
    [ "name"      A..= arName      ar
    , "breakdown" A..= arBreakdown ar
    ]

-- | A subaccount of one account.
data SubaccountReport = SubaccountReport
  { srName        :: T.Text
  , srAmount      :: Rational
  , srBalTag      :: T.Text
  , srURL         :: Maybe T.Text
  , srHistory     :: HistoryReport
  , srCommodities :: [(T.Text, CommodityBalanceReport)]
  } deriving Show

instance A.ToJSON SubaccountReport where
  toJSON sr = A.object $
    [ "name"     A..= srName sr
    , "amount"   A..= toDouble (srAmount sr)
    , "category" A..= srBalTag sr
    , "history"  A..= srHistory sr
    , "commodities" A..= A.object [ n A..= cr | (n, cr) <- srCommodities sr ]
    ] ++ maybe [] (\u -> [ "url" A..= u ]) (srURL sr)

-- | Information about a commodity in an account.
data CommodityBalanceReport = CommodityBalanceReport
  { cbrWorth  :: Rational
  , cbrAmount :: Rational
  } deriving Show

instance A.ToJSON CommodityBalanceReport where
  toJSON cbr = A.object
    [ "worth"  A..= toDouble (cbrWorth  cbr)
    , "amount" A..= toDouble (cbrAmount cbr)
    ]

-- | A list of balances.
newtype HistoryReport = HistoryReport
  { hrValues :: [(C.Day, Rational)]
  } deriving Show

instance A.ToJSON HistoryReport where
  toJSON hr = A.toJSON
    [ A.object [ "date" A..= date, "amount" A..= toDouble amount ]
    | (day, amount) <- hrValues hr
    , let date = C.formatTime C.defaultTimeLocale "%F" day
    ]

-- | A much simpler report than a full 'AccountReport'.
data BasicReport = BasicReport
  { brAccounts  :: [(T.Text, DeltaReport)]
  , brPriorDate :: C.Day
  } deriving Show

instance A.ToJSON BasicReport where
  toJSON br = A.object
    [ "accounts"   A..= A.object [ name A..= dr | (name, dr) <- brAccounts br ]
    , "prior_date" A..= T.pack (C.formatTime C.defaultTimeLocale "%F" (brPriorDate br))
    ]

-- | A report about the current and prior states of an account
data DeltaReport = DeltaReport
  { drCurrent :: Rational
  , drPrior   :: Rational
  , drHistory :: HistoryReport
  } deriving Show

instance A.ToJSON DeltaReport where
  toJSON dr = A.object
    [ "amount"  A..= toDouble (drCurrent dr)
    , "prior"   A..= toDouble (drPrior   dr)
    , "delta"   A..= toDouble (drCurrent dr - drPrior dr)
    , "history" A..= drHistory dr
    ]

-- | A report about a single transaction.
data TransactionReport = TransactionReport
  { trTitle :: T.Text
  , trDelta :: Rational
  } deriving Show

instance A.ToJSON TransactionReport where
  toJSON tr = A.object
    [ "title" A..= trTitle tr
    , "delta" A..= toDouble (trDelta tr)
    ]

-- | A history of transactions, grouped by day.
newtype DatedTransactionsReport = DatedTransactionsReport
  { dtrValues :: [(C.Day, [TransactionReport])]
  } deriving Show

instance A.ToJSON DatedTransactionsReport where
  toJSON dtr = A.object
    [ date A..= txns
    | (day, txns) <- dtrValues dtr
    , let date = T.pack (C.formatTime C.defaultTimeLocale "%F" day)
    ]

-- | Information about a commodity.
newtype CommodityReport = CommodityReport FC.CommodityConfig
  deriving Show

instance A.ToJSON CommodityReport where
  toJSON (CommodityReport cc) = A.object $ catMaybes
    [ ("name" A..=) <$> FC.cName cc
    , ("url"  A..=) <$> FC.cURL  cc
    , Just $ "allocation" A..= A.object [ n A..= w | (n, w) <- toList (FC.cAllocation cc) ]
    ]

-- | Turn a 'Rational' into a 'Double'.  This is lossy!
toDouble :: Rational -> Double
toDouble = fromRational
