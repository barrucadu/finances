{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow                 (second)
import           Control.Monad                 (join, mapM)
import           Data.Aeson                    (Value, object, (.=))
import qualified Data.Aeson                    as A
import           Data.Char                     (toUpper)
import           Data.List                     (inits, mapAccumL, nub)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, maybeToList)
import           Data.Ratio                    (Rational)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import qualified Data.Time.Calendar            as C
import qualified Data.Time.Clock               as C
import qualified Data.Time.Format              as C
import           Hledger.Data.Types            as H
import           Hledger.Read                  as H
import qualified Network.HTTP.Types.URI        as W
import qualified Network.HTTP.Types.Status     as W
import qualified Network.Wai                   as W
import qualified Network.Wai.Handler.Warp      as W
import qualified Network.Wai.Middleware.Static as W
import           Text.Read                     (readMaybe)

main :: IO ()
main = W.runEnv 5000 $ serveStatic (\req respond -> respond =<< serveDynamic req) where
  serveStatic = W.staticPolicy $ W.only [("", "static/index.html")] W.<|> W.addBase "static"
  serveDynamic req = fmap (W.responseLBS W.ok200 [] . A.encode) $ do
    today <- C.utctDay <$> C.getCurrentTime
    let qstr = map (second (fmap T.unpack)) . W.queryToQueryText . W.queryString $ req
        qyear  = readMaybe =<< join (lookup "year"  qstr)
        qmonth = readMaybe =<< join (lookup "month" qstr)
        qday   = readMaybe =<< join (lookup "day"   qstr)
    case qmonth of
      Just m ->
        let y = fromMaybe ((\(y,_,_) -> y) (C.toGregorian today)) qyear
            d = fromMaybe (C.gregorianMonthLength y m) qday
        in financeDataFor (C.fromGregorian y m d)
      _ -> financeDataFor today

-- | Get the data from the default hledger journal for the given date.
financeDataFor :: C.Day -> IO Value
financeDataFor today =
  dataFor today . H.jtxns <$> H.defaultJournal

-- | Get the data for a specific day.
dataFor :: C.Day -> [H.Transaction] -> Value
dataFor today txns = object
    [ "when"     .= T.pack (C.formatTime C.defaultTimeLocale "%B %_Y" today)
    , "assets"   .= balanceFrom (const epoch) assetAccount   id
    , "income"   .= balanceFrom monthStart    incomeAccount  negate
    , "budget"   .= balanceFrom (const epoch) budgetAccount  id
    , "expenses" .= balanceFrom monthStart    expenseAccount id
    , "history"  .= history
    ]
  where
    balanceFrom whenf accf valf = object
      [ account .= object [ "amount" .= toDouble (valf amount), "delta" .= toDouble delta, "prior" .= toDouble (valf old) ]
      | let currentBals = M.unionWith (-) (getBalances uptonow)   (balancesAt (whenf today)     uptonow)
      , let priorBals   = M.unionWith (-) (getBalances uptoprior) (balancesAt (whenf lastmonth) uptoprior)
      , (acc, amount) <- M.assocs currentBals
      , account <- maybeToList (accf acc)
      , let old = M.findWithDefault 0 acc priorBals
      , let delta = valf amount - valf old
      ]
    history = object
      [ date .= [ object [ "title" .= txntitle, "delta" .= toDouble assetDelta ]
                | (txntitle, txndeltas) <- reverse daytxns
                , let assetDelta = M.findWithDefault 0 "assets" txndeltas
                ]
      | (day, _, daytxns) <- takeWhile (\(d,_,_) -> monthYear d == monthYear today) uptonow
      , let date = T.pack (C.formatTime C.defaultTimeLocale "%d/%m" day)
      ]

    balancesAt cutoff = getBalances . dropWhile (\(d,_,_) -> d > cutoff)
    getBalances = (\(_,bals,_) -> bals) . headOr initial

    balances  = reverse (dailyBalances txns)
    uptonow   = dropWhile (\(d,_,_) -> d > today)     balances
    uptoprior = dropWhile (\(d,_,_) -> d > lastmonth) uptonow
    lastmonth = C.addGregorianMonthsClip (-1) today
    initial   = (epoch, M.empty, [])
    epoch     = C.fromGregorian 1970 1 1


-------------------------------------------------------------------------------

-- | Get the final balance after every day, and all the transaction
-- deltas on that day.
dailyBalances :: [H.Transaction] -> [(C.Day, M.Map T.Text Rational, [(T.Text, M.Map T.Text Rational)])]
dailyBalances = foldr squish [] . snd . mapAccumL process M.empty where
  process bals txn =
    let deltas = toDeltas txn
        bals'  = M.unionWith (+) bals deltas
    in (bals', (H.tdate txn, H.tdescription txn, bals', deltas))

  squish (day, desc, bals, delta) (o@(day2, bals2, txns):rest)
    | day == day2 = (day2, bals2, (desc, delta):txns) : rest
    | otherwise   = (day,  bals,  [(desc, delta)])    : o : rest
  squish (day, desc, bals, delta) [] = [(day, bals, [(desc, delta)])]

-- | Produce a collection of balance changes from a transaction.
toDeltas :: H.Transaction -> M.Map T.Text Rational
toDeltas txn =
    let postings = concatMap explodeAccount (H.tpostings txn)
        accounts = nub (map H.paccount postings)
    in M.fromList [ (a, val)
                  | a <- accounts
                  , let ps  = filter ((==a) . H.paccount) postings
                  , let val = sum (map (value . H.pamount) ps)
                  ]

-- | Convert a posting into a collection of posting to an account and
-- all of its superaccounts.
explodeAccount :: H.Posting -> [H.Posting]
explodeAccount p =
  [ p { H.paccount = a }
  | a <- tail . map (T.intercalate ":") . inits . T.splitOn ":" $ H.paccount p
  ]

-- | Get the value of an 'H.MixedAmount' as a 'Rational'.
value :: H.MixedAmount -> Rational
value (H.Mixed amounts) = sum (map go amounts) where
  go (H.Amount "£" q _ _) = toRational q
  go (H.Amount _   _ (H.TotalPrice a) _) = go a

-- | Get the head of a list, or a default value.
headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (x:_) = x

-- | Get the month and year of a 'C.Day'.
monthYear :: C.Day -> (Int, Integer)
monthYear d = let (y, m, _) = C.toGregorian d in (m, y)

-- | Get the end of the prior month.
monthStart :: C.Day -> C.Day
monthStart d = C.addDays (-1) (let (m, y) = monthYear d in C.fromGregorian y m 1)

-- | Turn a 'Rational' into a 'Double'.  This is lossy!
toDouble :: Rational -> Double
toDouble = fromRational

-------------------------------------------------------------------------------

-- | Give a nice name to an asset account.
assetAccount :: T.Text -> Maybe T.Text
assetAccount "assets:cavendish"  = Just "Cavendish Online"
assetAccount "assets:nationwide" = Just "Nationwide"
assetAccount "assets:santander:esaver" = Just "Santander (eSaver)"
assetAccount "assets:santander:main"   = Just "Santander (main)"
assetAccount _ = Nothing

-- | Give a nice name to an income account.
incomeAccount :: T.Text -> Maybe T.Text
incomeAccount "income:pta" = Just "PTA"
incomeAccount acc = simpleAccountName "income" acc

-- | Give a nice name to a budget account.
budgetAccount :: T.Text -> Maybe T.Text
budgetAccount = simpleAccountName "assets:santander:main:budget"

-- | Give a nice name to an expense account.
expenseAccount :: T.Text -> Maybe T.Text
expenseAccount = simpleAccountName "expenses"

-- | Check for a prefix and, if found, remove it and capitalise all
-- words.
simpleAccountName :: T.Text -> T.Text -> Maybe T.Text
simpleAccountName prefix acc = do
  name  <- T.stripPrefix (prefix <> ":") acc
  words <- mapM T.uncons (T.words name)
  pure . T.unwords $ map (\(c, rest) -> T.cons (toUpper c) rest) words
