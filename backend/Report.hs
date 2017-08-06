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
  { rpAssets      :: [AccountReport]
  , rpLiabilities :: [AccountReport]
  , rpIncome      :: BasicReport
  , rpBudget      :: BasicReport
  , rpExpenses    :: BasicReport
  , rpEquity      :: BasicReport
  , rpCommodities :: [(T.Text, CommodityReport)]
  } deriving Show

instance A.ToJSON Report where
  toJSON rp = A.object
    [ "assets"      A..= rpAssets      rp
    , "liabilities" A..= rpLiabilities rp
    , "income"      A..= rpIncome      rp
    , "budget"      A..= rpBudget      rp
    , "expenses"    A..= rpExpenses    rp
    , "equity"      A..= rpEquity      rp
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
  , srBalTag      :: T.Text
  , srURL         :: Maybe T.Text
  , srHistory     :: HistoryReport
  , srCommodities :: [(T.Text, CommodityHistoryReport)]
  } deriving Show

instance A.ToJSON SubaccountReport where
  toJSON sr = A.object $
    [ "name"     A..= srName sr
    , "category" A..= srBalTag sr
    , "history"  A..= srHistory sr
    , "commodities" A..= A.object [ n A..= cr | (n, cr) <- srCommodities sr ]
    ] ++ maybe [] (\u -> [ "url" A..= u ]) (srURL sr)

-- | Information about a commodity in an account.
data CommodityHistoryReport = CommodityHistoryReport
  { chrWorth  :: HistoryReport
  , chrAmount :: HistoryReport
  } deriving Show

instance A.ToJSON CommodityHistoryReport where
  toJSON chr = A.object
    [ "worth"  A..= chrWorth  chr
    , "amount" A..= chrAmount chr
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
newtype BasicReport = BasicReport
  { brAccounts  :: [(T.Text, HistoryReport)]
  } deriving Show

instance A.ToJSON BasicReport where
  toJSON br = A.object [ name A..= hr | (name, hr) <- brAccounts br ]

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
