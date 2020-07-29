{-# LANGUAGE CPP #-}

module Main where

import qualified Distribution.PackageDescription as PD

#if MIN_VERSION_Cabal (2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif

import qualified Distribution.Verbosity as PD
import qualified Distribution.Types.UnqualComponentName as PD
import qualified Distribution.ModuleName as PD
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.List (intercalate)

import Options.Applicative

import Data.Aeson as A

data Query = QueryTestNames
           | QueryTestModules
  deriving (Eq, Show)

data QueryOpt
  = QueryOpt { optCabalFile :: FilePath
             , optQuery     :: Query
             }
  deriving (Eq, Show)

queryOptsParser :: Parser QueryOpt
queryOptsParser =
  QueryOpt <$> strArgument (metavar "CABAL_FILE" <> help "Cabal file to query")
           <*> argument (eitherReader queryParser) (metavar "QUERY_STRING" <> help "Query string")

queryParser :: String -> Either String Query
queryParser "testNames"   = pure QueryTestNames
queryParser "testModules" = pure QueryTestModules
queryParser _ = Left "Supported queries are: testNames | testModules"

cqOptions :: ParserInfo QueryOpt
cqOptions = info (queryOptsParser <**> helper) (fullDesc <> progDesc "Query a cabal file." <> header "cq - a utility to query cabal files.")


queryTestNames :: PD.GenericPackageDescription -> [String]
queryTestNames = fmap testSuiteName' . PD.condTestSuites
  where
    testSuiteName' = PD.unUnqualComponentName . fst

queryTestModules :: PD.GenericPackageDescription -> [String]
queryTestModules gpd =
  let
    moduleName' :: PD.ModuleName -> String
    moduleName' = intercalate "." . PD.components
    testSuites' = fmap (PD.condTreeData . snd) . PD.condTestSuites
    testModules' = fmap moduleName' . PD.testModules
  in
    foldMap testModules' $ testSuites' gpd

mkAesonQuery :: Query -> PD.GenericPackageDescription -> A.Value
mkAesonQuery QueryTestNames   = A.toJSON . queryTestNames
mkAesonQuery QueryTestModules = A.toJSON . queryTestModules

main :: IO ()
main = do
  opts <- execParser cqOptions

  let
    cabalFile = optCabalFile opts
    query     = mkAesonQuery $ optQuery opts

  gpd <- readGenericPackageDescription PD.silent cabalFile
  let result = query gpd
  BSL.putStrLn (A.encode result)

  

