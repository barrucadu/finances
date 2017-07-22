{-# LANGUAGE OverloadedStrings #-}

module Report where

import qualified Data.Aeson         as A
import           Data.Ratio         (Rational)
import qualified Data.Text          as T
import qualified Data.Time.Calendar as C
import qualified Data.Time.Format   as C

-- | A report to send to the user.
data Report = Report
  { rpWhen :: C.Day
  , rpAssets :: [AccountReport]
  , rpLiabilities :: [AccountReport]
  , rpIncome :: [(T.Text, DeltaReport)]
  , rpBudget :: [(T.Text, DeltaReport)]
  , rpExpenses :: [(T.Text, DeltaReport)]
  , rpEquity :: [(T.Text, Rational)]
  , rpHistory :: [(C.Day, [TransactionReport])]
  } deriving Show

instance A.ToJSON Report where
  toJSON rp = A.object
    [ "when"        A..= T.pack (C.formatTime C.defaultTimeLocale "%B %_Y" (rpWhen rp))
    , "assets"      A..= A.toJSON (rpAssets      rp)
    , "liabilities" A..= A.toJSON (rpLiabilities rp)
    , "income"      A..= A.object [ name A..= A.toJSON dr | (name, dr) <- rpIncome   rp ]
    , "budget"      A..= A.object [ name A..= A.toJSON dr | (name, dr) <- rpBudget   rp ]
    , "expenses"    A..= A.object [ name A..= A.toJSON dr | (name, dr) <- rpExpenses rp ]
    , "equity"      A..= A.object [ name A..= A.toJSON (toDouble bal) | (name, bal) <- rpEquity rp ]
    , "history"     A..= A.object
      [ date A..= A.toJSON txns
      | (day, txns) <- rpHistory rp
      , let date = T.pack (C.formatTime C.defaultTimeLocale "%d/%m" day)
      ]
    ]

-- | A summary of an account.
data AccountReport = AccountReport
  { arName :: T.Text
  , arBreakdown :: [SubaccountReport]
  } deriving Show

instance A.ToJSON AccountReport where
  toJSON ar = A.object
    [ "name"      A..= arName      ar
    , "breakdown" A..= arBreakdown ar
    ]

-- | A subaccount of one account.
data SubaccountReport = SubaccountReport
  { srName :: T.Text
  , srAmount :: Rational
  , srTags :: [(T.Text, Int)]
  , srURL :: Maybe T.Text
  , srHistory :: HistoryReport
  } deriving Show

instance A.ToJSON SubaccountReport where
  toJSON sr = A.object $
    [ "name"   A..= srName sr
    , "amount" A..= toDouble (srAmount sr)
    , "tags"   A..= [ A.object [ "tag" A..= tag, "share" A..= share ]
                    | (tag, share) <- srTags sr
                    ]
    , "history" A..= A.toJSON (srHistory sr)
    ] ++ maybe [] (\u -> [ "url" A..= u ]) (srURL sr)

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

-- | A report about the current and prior states of an account
data DeltaReport = DeltaReport
  { drCurrent :: Rational
  , drPrior :: Rational
  , drHistory :: HistoryReport
  } deriving Show

instance A.ToJSON DeltaReport where
  toJSON dr = A.object
    [ "amount"  A..= toDouble (drCurrent dr)
    , "prior"   A..= toDouble (drPrior   dr)
    , "delta"   A..= toDouble (drCurrent dr - drPrior dr)
    , "history" A..= A.toJSON (drHistory dr)
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

-- | Turn a 'Rational' into a 'Double'.  This is lossy!
toDouble :: Rational -> Double
toDouble = fromRational
