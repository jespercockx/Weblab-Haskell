
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

output_file = "output/results.xml"

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
    [ "import Control.Monad"
    , "import Data.List"
    , "import System.Exit"
    , "import Test.QuickCheck"
    , lib
    , sol
    , tst
    , "main = do"
    ] ++
    [ "  r" ++ show i ++ " <- quickCheckResult " ++ p
    | (p,i) <- props
    ] ++
    [ "  let errs = [ unlines [\"Property \" ++ p ++ \":\"  , output r]"
    , "             | (p,r) <- " ++ allRs ++ ", not (isSuccess r) ]"
    , "  unless (null errs) $ die $ unlines $ intersperse \"=~*^=~*^=~*^=~*^\" errs"
    ]

  (exit, out, err) <- readProcessWithExitCode "runghc" ["runtests.hs"] ""

  let output
        | exit == ExitSuccess = successOutput
        | otherwise           = failureOutput $ prettyErrors err
  writeFile output_file $ XML.ppTopElement output

  where
    prettyErrors x =
      let errs = splitOn "=~*^=~*^=~*^=~*^" x
      in errs


-- Get the names of all QuickCheck properties in the file with the
-- given content (names starting with `prop_`).
getPropertyNames :: String -> [String]
getPropertyNames x =
  nub $ filter ("prop_" `isPrefixOf`) (map (fst . head . lex) (lines x))

successOutput :: XML.Element
successOutput =
  XML.unode "testsuites" $
  XML.unode "testsuite" $
  XML.unode "testcase" ()

failureOutput :: [String] -> XML.Element
failureOutput errs =
  XML.unode "testsuites" $
  XML.unode "testsuite" $
  XML.unode "testcase" $
  map (XML.unode "failure") errs
