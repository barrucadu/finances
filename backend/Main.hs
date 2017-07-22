{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow                 (second)
import           Control.Monad                 (join, mapM)
import qualified Data.Aeson                    as A
import           Data.Char                     (toUpper)
import           Data.List                     (inits, mapAccumL, nub, sortOn)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, listToMaybe,
                                                mapMaybe, maybeToList)
import           Data.Ratio                    (Rational)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import qualified Data.Time.Calendar            as C
import qualified Data.Time.Clock               as C
import qualified Data.Yaml                     as Y
import qualified Hledger.Data.Types            as H
import qualified Hledger.Read                  as H
import qualified Network.HTTP.Types.Status     as W
import qualified Network.HTTP.Types.URI        as W
import qualified Network.Wai                   as W
import qualified Network.Wai.Handler.Warp      as W
import qualified Network.Wai.Middleware.Static as W
import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure)
import           System.FilePath               ((</>), takeDirectory)
import           Text.Read                     (readMaybe)

import           Config
import           Report

main :: IO ()
main = do
  fp <- fromMaybe "config.yaml" . listToMaybe <$> getArgs
  config <- Y.decodeFileEither fp
  case config of
    Right cfg -> run (cfg { staticdir = takeDirectory fp </> staticdir cfg })
    Left err  -> do
      putStrLn ("Couldn't parse configuration file " ++ fp ++ ":")
      putStrLn (Y.prettyPrintParseException err)
      exitFailure


-------------------------------------------------------------------------------

-- | Run the web server.
run :: Config -> IO ()
run cfg = W.runEnv (port cfg) $ serveStatic (\req respond -> respond =<< serveDynamic req) where
  serveStatic = W.staticPolicy $ W.only [("", staticdir cfg </> "index.html")] W.<|> W.addBase (staticdir cfg)
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
        in financeDataFor cfg (C.fromGregorian y m d)
      _ -> financeDataFor cfg today

-- | Get the data from the default hledger journal for the given date.
financeDataFor :: Config -> C.Day -> IO A.Value
financeDataFor cfg today =
  dataFor cfg today . H.jtxns <$> H.defaultJournal

-- | Get the data for a specific day.
dataFor :: Config -> C.Day -> [H.Transaction] -> A.Value
dataFor cfg today txns = A.toJSON Report
    { rpWhen        = today
    , rpAssets      = accountsReport (assetAccounts     cfg) (const "Current")
    , rpLiabilities = accountsReport (liabilityAccounts cfg) id
    , rpIncome      = balanceFrom monthStart    (account (incomeRules  cfg))
    , rpBudget      = balanceFrom (const epoch) (account (budgetRules  cfg))
    , rpExpenses    = balanceFrom monthStart    (account (expenseRules cfg))
    , rpEquity      = simpleReport (account (equityRules cfg))
    , rpHistory     = history
    }
  where
    accountsReport accounts baltagf =
      [ AccountReport
        { arName      = accName acc
        , arBreakdown =
            [ SubaccountReport
              { srName    = fromMaybe (accName acc) (subName subacc)
              , srAmount  = amount
              , srTags    = subTag subacc
              , srBalTag  = fromMaybe (baltagf (accName acc)) (subBalTag subacc)
              , srURL     = subURL subacc
              , srHistory = allHistory (subHledgerAccount subacc)
              }
            | subacc <- accBreakdown acc
            , let amount = M.findWithDefault 0 (subHledgerAccount subacc) (getBalances uptonow)
            ]
        }
      | acc <- accounts
      ]

    balanceFrom whenf accf = BasicReport
      { brAccounts =
          [ (account, DeltaReport { drCurrent = amount, drPrior = old, drHistory = allHistory acc })
          | let currentBals = M.unionWith (-) (getBalances uptonow)   (balancesAt (whenf today)     uptonow)
          , let priorBals   = M.unionWith (-) (getBalances uptoprior) (balancesAt (whenf lastmonth) uptoprior)
          , acc <- M.keys (getBalances balances)
          , let amount = M.findWithDefault 0 acc currentBals
          , account <- maybeToList (accf acc)
          , let old = M.findWithDefault 0 acc priorBals
          ]
      , brPriorDate = lastmonth
      }

    simpleReport accf =
      [ (account, amount)
      | let currentBals = getBalances uptonow
      , (acc, amount) <- M.assocs currentBals
      , account <- maybeToList (accf acc)
      ]

    history =
      [ (day, [ TransactionReport { trTitle = txntitle, trDelta = assetDelta }
              | (txntitle, txndeltas) <- reverse daytxns
              , let assetDelta = M.findWithDefault 0 "assets" txndeltas
              ])
      | (day, _, daytxns) <- takeWhile (\(d,_,_) -> monthYear d == monthYear today) uptonow
      ]

    allHistory account =
      let amountIn = M.findWithDefault 0 account
      in HistoryReport { hrValues = [ (day, amountIn bals) | (day, bals, _) <- balancesAsc ] }

    balancesAt cutoff = getBalances . dropWhile (\(d,_,_) -> d > cutoff)
    getBalances = (\(_,bals,_) -> bals) . headOr initial

    balancesAsc = dailyBalances txns
    balances  = reverse balancesAsc
    uptonow   = dropWhile (\(d,_,_) -> d > today)     balances
    uptoprior = dropWhile (\(d,_,_) -> d > lastmonth) uptonow
    lastmonth = C.addGregorianMonthsClip (-1) today
    initial   = (epoch, M.empty, [])
    epoch     = C.fromGregorian 1970 1 1


-------------------------------------------------------------------------------

-- | Get the final balance after every day, and all the transaction
-- deltas on that day.
dailyBalances :: [H.Transaction] -> [(C.Day, M.Map T.Text Rational, [(T.Text, M.Map T.Text Rational)])]
dailyBalances = foldr squish [] . snd . mapAccumL process M.empty . sortOn H.tdate where
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

-- | Give a nice name to an account.
account :: AccountRules -> T.Text -> Maybe T.Text
account rules0 acc = go rules0 where
  go (Only whitelist) = lookup acc whitelist
  go (Try rules) = listToMaybe (mapMaybe go rules)
  go (Simple prefix)  = do
    name  <- T.stripPrefix (prefix <> ":") acc
    words <- mapM T.uncons (T.words name)
    pure . T.unwords $ map (\(c, rest) -> T.cons (toUpper c) rest) words
  go None = Nothing
