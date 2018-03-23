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
import           Data.Maybe                (catMaybes, maybeToList)
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           System.Directory          (doesPathExist, getCurrentDirectory,
                                            listDirectory)
import           System.Exit               (exitFailure, exitSuccess)
import           System.FilePath           (FilePath, splitPath, (</>))
import           Text.Regex.Base.RegexLike (defaultCompOpt, defaultExecOpt,
                                            matchTest)
import           Text.Regex.TDFA.String

data Config = Config
  { fixedString   :: Bool
  , showDirectory :: Bool
  , keepGoing     :: Bool
  , query         :: String
  }

main :: IO ()
main = do
  config <- execParser options

  let display (d, fs) = if showDirectory config
                          then putStrLn d
                          else traverse_ (putStrLn . (d </>)) fs

  match <- if fixedString config
             then pure $ fmap (fmap pure) . fixedMatch (query config)
             else case compile defaultCompOpt defaultExecOpt (query config) of
                    Left err -> do
                      putStrLn ("error compiling regex: " ++ err)
                      exitFailure
                    Right regex -> pure $ regexMatch regex

  let search = if keepGoing config
                 then findAllAncestors
                 else fmap maybeToList . findFirstAncestor

  search match >>= \case
    [] -> exitFailure
    xs -> traverse_ display xs >> exitSuccess

--------------------------------------------------------------------------------
-- File matchers
--------------------------------------------------------------------------------

fixedMatch :: String -> FilePath -> IO (Maybe FilePath)
fixedMatch f d = ifM (doesPathExist (d </> f))
                   (pure $ Just f)
                   (pure Nothing)

regexMatch :: Regex -> FilePath -> IO (Maybe (NonEmpty FilePath))
regexMatch r d = do
  matches <- filter (matchTest r) <$> listDirectory d
  pure $ nonEmpty matches

--------------------------------------------------------------------------------
-- searching functions
--------------------------------------------------------------------------------

findFirstAncestor :: (FilePath -> IO (Maybe a)) -> IO (Maybe (FilePath, a))
findFirstAncestor f =
  firstJustM (\d -> fmap (d,) <$> f d) =<< getAncestors

findAllAncestors :: (FilePath -> IO (Maybe a)) -> IO [(FilePath, a)]
findAllAncestors f =
  fmap catMaybes . traverse (\d -> fmap (d,) <$> f d) =<< getAncestors

getAncestors :: IO [FilePath]
getAncestors = reverse
               . fmap mconcat
               . tail
               . inits
               . splitPath
               <$> getCurrentDirectory

--------------------------------------------------------------------------------
-- option parsing
--------------------------------------------------------------------------------

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
                       , help "Show the directory containing matches instead of the matches themselves"
                       ]
                 )
      <*> switch (fold [ long "keep-going"
                       , short 'k'
                       , help "Don't stop after finding the first directory with matching files"
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
