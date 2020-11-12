
import Control.Monad

import Data.List hiding ( find )
import Data.List.Split
import Data.Maybe

import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Find
import System.IO
import System.Process

import qualified Text.XML.Light as XML

import FormatResult

outputFile = "output/results.xml"

-- Run the tests on the solution in the current directory.
-- Precondition: files `library.hs`, `solution.hs`, and `test.hs` are present.
-- Return @Nothing@ if successful or @Just err@ in case of error.
main :: IO ()
main = do
  lib <- readFile "library.hs"
  sol <- readFile "solution.hs"
  tst <- readFile "test.hs"

  let props = zip (getPropertyNames tst) [0..]
  let allRs = "[" ++ intercalate "," (map (\(p,i) -> "(\"" ++ p ++ "\",r" ++ show i ++ ")") props) ++ "]"

  writeFile "runtests.hs" $ unlines $
    [ "import Test.QuickCheck"
    , "import FormatResult"
    , lib
    , sol
    , tst
    , "main = do"
    ] ++
    [ "  r" ++ show i ++ " <- quickCheckResult " ++ p
    | (p,i) <- props
    ] ++
    [ "  writeResults " ++ show outputFile ++ " " ++ allRs ]

  (exit, out, err) <- readProcessWithExitCode "runghc" ["runtests.hs"] ""

  -- If runghc threw an error, write it to the output as a single failure.
  when (exit /= ExitSuccess) $
    writeFile outputFile $ XML.ppTopElement $ failureOutput err

  where
    failureOutput :: String -> XML.Element
    failureOutput err =
      XML.unode "testsuites" $
      XML.unode "testsuite" $
      XML.unode "testcase" $
      XML.unode "failure" err

-- Get the names of all QuickCheck properties in the file with the
-- given content (names starting with `prop_`).
getPropertyNames :: String -> [String]
getPropertyNames x =
  nub $ filter ("prop_" `isPrefixOf`) (map (fst . head . lex) (lines x))
