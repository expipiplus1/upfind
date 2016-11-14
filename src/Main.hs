{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}

module Main
  ( main
  ) where

import           Control.Monad.Extra
import           Data.Foldable             (fold, traverse_)
import           Data.List                 (inits)
import           Data.List.NonEmpty        (NonEmpty (..), nonEmpty)
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           System.Directory          (doesFileExist, getCurrentDirectory,
                                            getDirectoryContents)
import           System.Exit               (exitFailure, exitSuccess)
import           System.FilePath           (FilePath, splitPath, (</>))
import           Text.Regex.Base.RegexLike (defaultCompOpt, defaultExecOpt,
                                            matchTest)
import           Text.Regex.TDFA.String

data Config = Config
  { fixedString   :: Bool
  , showDirectory :: Bool
  , query         :: String
  }

main :: IO ()
main = do
  config <- execParser options

  let display (d, fs) = if showDirectory config
                          then putStrLn d
                          else traverse_ (putStrLn . (d </>)) fs

  match <- if fixedString config
             then pure $ fixedMatch (query config)
             else case compile defaultCompOpt defaultExecOpt (query config) of
                    Left err -> do
                      putStrLn ("error compiling regex: " ++ err)
                      exitFailure
                    Right regex -> pure $ regexMatch regex

  searchAncestors match >>= \case
    Nothing -> exitFailure
    Just r  -> display r >> exitSuccess

fixedMatch :: String -> FilePath -> IO (Maybe (NonEmpty FilePath))
fixedMatch f d = ifM (doesFileExist (d </> f))
                   (pure $ Just (f :| []))
                   (pure Nothing)

regexMatch :: Regex -> FilePath -> IO (Maybe (NonEmpty FilePath))
regexMatch r d = do
  matches <- filter (matchTest r) <$> getDirectoryContents d
  pure $ nonEmpty matches

searchAncestors :: (FilePath -> IO (Maybe a)) -> IO (Maybe (FilePath, a))
searchAncestors f = do
  ancestors <- reverse
               . fmap mconcat
               . tail
               . inits
               . splitPath
               <$> getCurrentDirectory
  firstJustM (\d -> fmap (d,) <$> f d) ancestors

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser = Config
      <$> switch (fold [ long "fixed"
                       , short 'F'
                       , help "Match on a fixed string not a regex"
                       ]
                 )
      <*> switch (fold [ long "show-directory"
                       , short 'd'
                       , help "show the closest directory containing a match"
                       ]
                 )
      <*> argument str (fold [ metavar "PATTERN"
                             , help "pattern to search for"
                             ]
                       )

    description = fold
      [ fullDesc
      , header "upfind"
      , progDesc "A program to search upwards for files"
      ]
