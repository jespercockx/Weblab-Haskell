
import Control.Monad

import Data.Hashable ( hash )
import Data.List ( intercalate, nub, isPrefixOf )
import Data.Maybe ( fromMaybe, listToMaybe )

import System.Exit ( ExitCode(..) )
import System.IO ( readFile' )
import System.Process ( readProcessWithExitCode )
import Text.Read ( readMaybe )

import qualified Text.XML.Light as XML

import FormatResult

outputFile = "output/results.xml"

-- Run the tests on the solution in the current directory.
-- Precondition: files `library.hs`, `solution.hs`, and `test.hs` are present.
-- Return @Nothing@ if successful or @Just err@ in case of error.
main :: IO ()
main = do
  lib <- readFile' "library.hs"
  sol <- readFile' "solution.hs"
  tst <- readFile' "test.hs"

  writeFile "Library.hs"  $ unlines [ "module Library where" ] ++ lib
  writeFile "Solution.hs" $ unlines [ "module Solution where" , "import Library" ] ++ sol
  writeFile "Test.hs"     $ unlines [ "module Test where" , "import Test.QuickCheck", "import Library" , "import Solution" ] ++ tst

  let props = zipWith (\(p,w) i -> (p,w,i)) (getPropertyNamesAndWeights tst)  [0..]
  let allRs = "[" ++ intercalate "," (map (\(p,w,i) -> "(\"" ++ p ++ "\"," ++ show w ++ ",r" ++ show i ++ ")") props) ++ "]"
  let qcArgs = case splitAt 8 (fromMaybe "" $ listToMaybe $ lines tst) of
        ("-- seed:", x) -> \u -> "stdArgs { replay = Just (mkQCGen (" ++ show (hash (u,x)) ++ "), 0) }"
        _               -> const "stdArgs"

  writeFile "runtests.hs" $ unlines $
    [ "import Test.QuickCheck"
    , "import Test.QuickCheck.Random"
    , "import FormatResult"
    , "import Test"
    , "main = do"
    ] ++
    [ "  r" ++ show i ++ " <- quickCheckWithResult " ++ qcArgs p ++ " " ++ p
    | (p,w,i) <- props
    ] ++
    [ "  writeResults " ++ show outputFile ++ " " ++ allRs ]

  (exit, out, err) <- readProcessWithExitCode "runghc" ["-dynamic", "runtests.hs"] ""

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

-- Get the names and weights of all QuickCheck properties in the file
-- with the given content (names starting with `prop_` or with
-- `prop_x_` for a test with weight x).
getPropertyNamesAndWeights :: String -> [(String,Int)]
getPropertyNamesAndWeights x =
  let props   = nub $ filter ("prop_" `isPrefixOf`) (map (fst . head . lex) (lines x))
      weights = map getWeight props
  in zip props weights
  where
    getWeight p = case readMaybe (takeWhile (/= '_') (drop 5 p)) of
      Just (n :: Int) -> n
      Nothing         -> 1
