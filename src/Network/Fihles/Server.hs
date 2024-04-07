{-# LANGUAGE OverloadedStrings #-}

module Network.Fihles.Server
  ( runServer,
  )
where

import Control.Monad (forM_)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentDisposition)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory.FileGrabber (getDirectoryEntries)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H

app :: Application
app req respond = do
  putStrLn "Starting Server"
  let path = Text.intercalate "/" $ pathInfo req
  entries <- getDirectoryEntries
  if path == ""
    then do respond $ responseLBS status200 [("Content-Type", "text/html")] $ renderHtml $ generateIndex entries
    else do
      respond $
        responseFile
          status200
          [(hContentType, "text/plain"), (hContentDisposition, "attachment; filename=" <> encodeUtf8 path)]
          (Text.unpack path)
          Nothing

runServer :: IO ()
runServer = do
  putStrLn "http://localhost:8080/"
  run 8080 app

generateIndex :: [FilePath] -> Html
generateIndex files = docTypeHtml $ do
  H.head $ do
    H.title "Index for /"
  body $ do
    p "Available files"
    ul $ forM_ files (li . toHtml)
