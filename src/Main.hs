{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad.Extra
import           Data.List                 (inits)
import           Data.Text                 (Text, pack)
import qualified Data.Text.IO              as T
import           Filesystem.Path           (FilePath, splitDirectories, (</>))
import           Filesystem.Path.CurrentOS (encodeString, fromText)
import           Prelude                   hiding (FilePath)
import           System.Environment        (getArgs)
import           System.Exit               (exitFailure, exitSuccess)
import           Turtle.Prelude

main :: IO ()
main = getArgs >>= \case
  [filename] ->
    upfind (pack filename) >>= \case
      Nothing -> exitFailure
      Just f -> do
        putStrLn . encodeString $ f
        exitSuccess
  _ -> do
    T.putStrLn usage
    exitFailure

usage :: Text
usage = "Usage: upfind filename"

upfind :: Text -> IO (Maybe FilePath)
upfind t = do
  let f = fromText t
      hasFile d = testfile (d </> f)
  ancestors <- reverse
               . fmap mconcat
               . tail
               . inits
               . splitDirectories
               <$> pwd
  fmap (</> f) <$> findM hasFile ancestors
