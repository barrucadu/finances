{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Template where

import qualified Data.Default.Class as D
import qualified Data.Text          as T
import           System.FilePath    (FilePath, (<.>), (</>))
import           System.IO.Error    (tryIOError)
import qualified Text.Ginger        as G
import qualified Text.Ginger.Html   as G

-- | The summary / index page.
summaryPage :: IO (Either T.Text T.Text)
summaryPage = loadAndRenderTemplate "index"

-- | The balance sheet page.
balancesheetPage :: IO (Either T.Text T.Text)
balancesheetPage = loadAndRenderTemplate "balancesheet"

-- | The cashflow page.
cashflowPage :: IO (Either T.Text T.Text)
cashflowPage = loadAndRenderTemplate "cashflow"

-- | The history page.
historyPage :: IO (Either T.Text T.Text)
historyPage = loadAndRenderTemplate "history"


-------------------------------------------------------------------------------
-- Template utilities

-- | Load and render a Ginger template, or return an error.
loadAndRenderTemplate :: String -> IO (Either T.Text T.Text)
loadAndRenderTemplate tplname =
  fmap (renderTemplate tplname) <$> loadTemplate tplname

-- | Render a Ginger template.
renderTemplate :: String -> G.Template -> T.Text
renderTemplate tplname tpl =
  let ctx = G.makeContextHtml $ \case
        "page" -> G.toGVal (T.pack tplname)
        _      -> D.def
  in G.htmlSource (G.runGinger ctx tpl)

-- | Load a Ginger template, or return an error.
loadTemplate :: String -> IO (Either T.Text G.Template)
loadTemplate tplname =
  either (Left . T.pack . G.formatParserError Nothing) Right <$> G.parseGingerFile resolveInclude tplname

-- | Read a file, returning @Nothing@ on failure.
resolveInclude :: G.IncludeResolver IO
resolveInclude tplname =
  either (const Nothing) Just <$> tryIOError (readFile (toTemplatePath tplname))

-- | Given a template file name, give its path.
toTemplatePath :: String -> FilePath
toTemplatePath tplname = "tpl" </> tplname <.> "tpl"
