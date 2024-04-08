{-# LANGUAGE OverloadedStrings #-}

module Network.Fihles.Server
  ( runServer,
  )
where

import qualified Data.ByteString.UTF8 as BS
import Data.List (intercalate, sort)
import qualified Data.Text as Text
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentDisposition)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html, a, docTypeHtml, h1, hr, li, stringValue, title, toHtml, ul, (!))
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes (href)
import Text.Printf (printf)

app :: Application
app request respond = do
  putStrLn "Starting Server"
  printf "Path info is %s\n" $ show $ pathInfo request
  let path = intercalate [pathSeparator] . map Text.unpack $ pathInfo request
  fullPath <- fmap (</> path) getCurrentDirectory
  printf "Path is %s\n" path
  printf "Full Path is %s\n" fullPath
  isDir <- doesDirectoryExist fullPath
  isFile <- doesFileExist fullPath
  if isDir
    then do
      entries <- sort <$> listDirectory fullPath
      respond . responseLBS status200 [("Content-Type", "text/html")] . renderHtml $ generateHtml path entries
    else
      if isFile
        then do
          respond $
            responseFile
              status200
              [(hContentType, "text/plain"), (hContentDisposition, "attachment; filename=" <> BS.fromString (takeFileName path))]
              path
              Nothing
        else do respond $ responseLBS status404 [("Content-Type", "text/plain")] "File not found"

runServer :: IO ()
runServer = do
  putStrLn "Find you files at http://localhost:8080/"
  run 8080 app

generateHtml :: FilePath -> [FilePath] -> Html
generateHtml currentPath files = docTypeHtml $ do
  Html.head $ do
    title "Fihles"
  Html.body $ do
    let displayedPath = [pathSeparator] </> currentPath
    h1 . toHtml $ "Directory listing for " <> displayedPath
    hr
    ul $ mapM_ (generateEntry displayedPath) files
    hr

generateEntry :: FilePath -> FilePath -> Html
generateEntry directory file = li (a ! href (stringValue $ directory </> file) $ toHtml file)
