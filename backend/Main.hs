{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Arrow                 (second)
import qualified Data.Aeson                    as A
import           Data.Char                     (toUpper)
import           Data.List                     (inits, isPrefixOf, mapAccumL,
                                                nub, sortOn)
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, listToMaybe)
import           Data.Ratio                    (Rational)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Time.Calendar            as C
import qualified Data.Yaml                     as Y
import qualified Hledger.Data.Types            as H
import qualified Hledger.Read                  as H
import qualified Network.HTTP.Types.Status     as W
import qualified Network.Wai                   as W
import qualified Network.Wai.Handler.Warp      as W
import qualified Network.Wai.Middleware.Static as W
import           System.Directory              (getHomeDirectory)
import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure)
import           System.FilePath               (FilePath, takeDirectory, (</>))

import qualified Config                        as FC
import qualified Report                        as FR
import qualified Template                      as FT

main :: IO ()
main = do
  fp <- fromMaybe "config.yaml" . listToMaybe <$> getArgs
  config <- Y.decodeFileEither fp
  case config of
    Right cfg -> do
      let baseDir = takeDirectory fp
      staticDir   <- canonicalise baseDir (FC.staticDir (FC.http cfg))
      journalPath <- maybe (pure Nothing) (fmap Just . canonicalise baseDir) (FC.journalPath cfg)
      let cfg' = cfg { FC.journalPath = journalPath, FC.http = (FC.http cfg) { FC.staticDir = staticDir } }
      run cfg'
    Left err  -> do
      putStrLn ("Couldn't parse configuration file " ++ fp ++ ":")
      putStrLn (Y.prettyPrintParseException err)
      exitFailure


-------------------------------------------------------------------------------

-- | Run the web server.
run :: FC.Config -> IO ()
run cfg = W.runEnv (FC.port . FC.http $ cfg) $ serveStatic (\req respond -> respond =<< serveDynamic req) where
  serveStatic =
    let staticdir = FC.staticDir (FC.http cfg)
    in W.staticPolicy (W.addBase staticdir)

  serveDynamic req = do
    journalPath <- maybe H.defaultJournalPath pure (FC.journalPath cfg)
    H.readJournalFile Nothing Nothing True journalPath >>= \case
      Right journal -> serveDynamic' req journal
      Left  err -> do
        putStrLn err
        pure (W.responseLBS W.internalServerError500 [] "cannot read journal file")

  serveDynamic' req journal = case W.pathInfo req of
    []                    -> serveTemplate <$> FT.summaryPage
    ["index.html"]        -> serveTemplate <$> FT.summaryPage
    ["balancesheet.html"] -> serveTemplate <$> FT.balancesheetPage
    ["cashflow.html"]     -> serveTemplate <$> FT.cashflowPage
    ["history.html"]      -> serveTemplate <$> FT.historyPage

    ["data"]    -> pure $ serveJSON (A.toJSON . accountsReport cfg        $ H.jtxns journal)
    ["history"] -> pure $ serveJSON (A.toJSON . historyReport  cfg . Left $ H.jtxns journal)

    _ -> pure $ W.responseLBS W.notFound404 [] "not found"

  serveJSON = W.responseLBS W.ok200 [] . A.encode

  serveTemplate (Right html) = W.responseBuilder W.ok200 [] (T.encodeUtf8Builder html)
  serveTemplate (Left  perr) = W.responseBuilder W.internalServerError500 [] (T.encodeUtf8Builder perr)

-- | Get the account historical balances.
accountsReport :: FC.Config -> [H.Transaction] -> FR.Report
accountsReport cfg txns = FR.Report
    { FR.rpAssets      = map accountReport assetAccounts
    , FR.rpLiabilities = map accountReport liabilityAccounts
    , FR.rpIncome      = balanceFrom incomeAccounts
    , FR.rpBudget      = balanceFrom budgetAccounts
    , FR.rpExpenses    = balanceFrom expenseAccounts
    , FR.rpEquity      = balanceFrom equityAccounts
    , FR.rpCommodities = map (second FR.CommodityReport) (FC.commodities cfg)
    }
  where
    accounts          = sortOn (accountName cfg) $ M.keys (getBalances balances)
    assetAccounts     = matching False (FC.assetAccounts     . FC.tree $ cfg) accounts
    budgetAccounts    = matching False (FC.budgetAccounts    . FC.tree $ cfg) accounts
    equityAccounts    = matching False (FC.equityAccounts    . FC.tree $ cfg) accounts
    expenseAccounts   = matching False (FC.expenseAccounts   . FC.tree $ cfg) accounts
    incomeAccounts    = matching False (FC.incomeAccounts    . FC.tree $ cfg) accounts
    liabilityAccounts = matching False (FC.liabilityAccounts . FC.tree $ cfg) accounts

    -- todo: factor this out to a new top-level definition
    accountReport account = FR.AccountReport
      { FR.arName      = accountName cfg account
      , FR.arBreakdown =
          [ FR.SubaccountReport
            { FR.srName        = accountName     cfg subaccount
            , FR.srBalTag      = accountCategory cfg subaccount
            , FR.srURL         = accountURL      cfg subaccount
            , FR.srHistory     = allHistory          subaccount
            , FR.srCommodities = [ (c, commodityHistory subaccount c)
                                 | c <- commoditiesIn subaccount
                                 ]
            }
          | let subaccounts = filter (`isDirectSubaccountOf` account) accounts
          , subaccount <- if null subaccounts then [account] else subaccounts
          ]
      }

    -- todo: factor this out to a new top-level definition
    balanceFrom accounts = FR.BasicReport
      { FR.brAccounts =
          [ (accountName cfg account, allHistory account) | account <- accounts ]
      }

    commoditiesIn account = M.keys (M.findWithDefault M.empty account (getCommodities balances))

    allHistory account =
      let amountIn = M.findWithDefault 0 account
      in FR.HistoryReport { FR.hrValues = [ (dbDay db, amountIn (dbBalances db)) | db <- balancesAsc ] }

    commodityHistory account c =
      let amountIn = M.findWithDefault (0, 0) c . M.findWithDefault M.empty account
          history  = [ (dbDay db, amountIn (dbCommodities db)) | db <- balancesAsc ]
          report f = FR.HistoryReport [ (day, f x)  | (day, x)  <- history ]
      in FR.CommodityHistoryReport { FR.chrWorth  = report fst
                                   , FR.chrAmount = report snd
                                   }

    getBalances    = dbBalances . headOr initial
    getCommodities = dbCommodities . headOr initial

    balancesAsc = dailyBalances cfg txns
    balances = reverse balancesAsc
    initial  = DailyBalance epoch M.empty []
    epoch    = C.fromGregorian 1970 1 1

-- | Get the transaction history.
historyReport :: FC.Config -> Either [H.Transaction] [DailyBalance] -> FR.DatedTransactionsReport
historyReport cfg txnbals = FR.DatedTransactionsReport
    [ ( dbDay bal
      , [ FR.TransactionReport { FR.trTitle = txntitle, FR.trDelta = assetDelta }
        | (txntitle, txndeltas) <- reverse (dbTxns bal)
        , let assetDelta = M.findWithDefault 0 "assets" txndeltas
        ]
      )
    | bal <- balances
    ]
  where
    balances = case txnbals of
      Right bals -> bals
      Left txns -> reverse (dailyBalances cfg txns)


-------------------------------------------------------------------------------

-- | The events of one day.
data DailyBalance = DailyBalance
  { dbDay         :: C.Day
  , dbCommodities :: M.Map T.Text (M.Map T.Text (Rational, Rational))
  , dbTxns        :: [(T.Text, M.Map T.Text Rational)]
  } deriving Show

-- | Get a set of balance deltas from a 'DailyBalance'.
dbBalances :: DailyBalance -> M.Map T.Text Rational
dbBalances db = (sum . map fst . M.elems) <$> dbCommodities db

-- | Get the final balance after every day, and all the transaction
-- deltas on that day.
dailyBalances :: FC.Config -> [H.Transaction] -> [DailyBalance]
dailyBalances cfg = foldr squish [] . snd . mapAccumL process M.empty . sortOn H.tdate where
  process comms txn =
    let cdeltas = toDeltas cfg txn
        bdeltas = (sum . map fst . M.elems) <$> cdeltas
        comms'  = M.unionWith (M.unionWith (\(a,b) (c,d) -> (a+c, b+d))) comms cdeltas
    in (comms', (H.tdate txn, H.tdescription txn, comms', bdeltas))

  squish x@(day, desc, _, delta) (db:rest)
    | day == dbDay db = db { dbTxns = (desc, delta) : dbTxns db } : rest
    | otherwise = makedb x : db : rest
  squish x [] = [makedb x]

  makedb (day, desc, comms, delta) = DailyBalance
    { dbDay         = day
    , dbCommodities = comms
    , dbTxns        = [(desc, delta)]
    }

-- | Produce a collection of commodity-level balance changes from a
-- transaction.
toDeltas :: FC.Config -> H.Transaction -> M.Map T.Text (M.Map T.Text (Rational, Rational))
toDeltas cfg txn =
    let postings = concatMap explodeAccount (H.tpostings txn)
        accounts = nub (map H.paccount postings)
        val a = fromMaybe (error ("amount which reduced to non-default commodity: " ++ show a)) (value cfg a)
        qty   = toRational . H.aquantity
    in M.fromList [ (a, M.unionsWith (\(a,b) (c,d) -> (a+c, b+d)) cs)
                  | a <- accounts
                  , let ps  = filter ((==a) . H.paccount) postings
                  , let cs = [ M.singleton (H.acommodity a) (val a, qty a)
                             | H.Mixed [a] <- map H.pamount ps
                             ]
                  ]

-- | Convert a posting into a collection of posting to an account and
-- all of its superaccounts.
explodeAccount :: H.Posting -> [H.Posting]
explodeAccount p =
  [ p { H.paccount = a }
  | a <- tail . map (T.intercalate ":") . inits . T.splitOn ":" $ H.paccount p
  ]

-- | Get the value of an 'H.Amount' as a 'Rational'.
value :: FC.Config -> H.Amount -> Maybe Rational
value cfg = go where
  go (H.Amount c q p _)
    | c == FC.defcommodity cfg = Just (toRational q)
    | otherwise = case p of
        H.TotalPrice a -> go a
        H.UnitPrice  a -> (toRational q *) <$> go a
        H.NoPrice -> Nothing

-- | Get the head of a list, or a default value.
headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (x:_) = x

-- | Give a nice name to an account, defaults to the bit after the
-- last ":" in titlecase.
accountName :: FC.Config -> T.Text -> T.Text
accountName cfg acc = fromMaybe def $ FC.aName =<< lookup acc (FC.accounts cfg) where
  def =
    let bits = T.words $ last (T.splitOn ":" acc)
        cap w = case T.uncons w of
          Just (c, cs) -> T.cons (toUpper c) cs
          Nothing -> ""
    in T.unwords (map cap bits)

-- | Get the category of an account, with defaulting.
accountCategory :: FC.Config -> T.Text -> T.Text
accountCategory cfg acc = fromMaybe def $ FC.aCategory =<< lookup acc (FC.accounts cfg) where
  def | matches True (FC.assetAccounts     tree) acc = "Current"
      | matches True (FC.equityAccounts    tree) acc = "Start of Period"
      | matches True (FC.liabilityAccounts tree) acc = "Current"
      | otherwise = "End of Period"

  tree = FC.tree cfg

-- | Get the URL of an account.
accountURL :: FC.Config -> T.Text -> Maybe T.Text
accountURL cfg acc = FC.aURL =<< lookup acc (FC.accounts cfg)

-- | Expand relative paths and a "~/" at the start of a path.
canonicalise :: FilePath -> FilePath -> IO FilePath
canonicalise _ ('~':'/':rest) = (</> rest) <$> getHomeDirectory
canonicalise basedir (fp@(c:_))
  | c /= '/' = pure (basedir </> fp)
canonicalise _ fp = pure fp

-- | Get account names matching any of a collection of patterns.
matching :: Bool -> NonEmpty FC.Pattern -> [T.Text] -> [T.Text]
matching subaccounts = filter . matches subaccounts

-- | Check if an account name matches any of a collection of patterns.
matches :: Bool -> NonEmpty FC.Pattern -> T.Text -> Bool
matches subaccounts pats acc = or (fmap go pats) where
  accBits = T.splitOn ":" acc

  go (FC.Pattern pat) =
    let patBits = T.splitOn ":" pat
        check p a = p == "*" || p == a
    in and (zipWith check patBits accBits)
       && (if subaccounts
           then length patBits <= length accBits
           else length patBits == length accBits)

-- | Check if an account is a direct subaccount of another.
isDirectSubaccountOf :: T.Text -> T.Text -> Bool
isDirectSubaccountOf sub acc =
  let subBits = T.splitOn ":" sub
      accBits = T.splitOn ":" acc
  in 1 + length accBits == length subBits && accBits `isPrefixOf` subBits
