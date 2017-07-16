{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.HTTP.Types.Status     (ok200)
import           Network.Wai                   (responseLBS)
import           Network.Wai.Handler.Warp      (runEnv)
import           Network.Wai.Middleware.Static (addBase, only, staticPolicy,
                                                (<|>))

main :: IO ()
main = runEnv 5000 . serveStatic $ \_ respond -> (respond $ responseLBS ok200 [] "Hello world") where
  serveStatic = staticPolicy $ only [("", "static/index.html")] <|> addBase "static"
