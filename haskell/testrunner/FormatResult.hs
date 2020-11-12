
module FormatResult where

import Test.QuickCheck
import qualified Text.XML.Light as XML

formatResult :: String -> Result -> XML.Element
formatResult prop r
  | isSuccess r = XML.unode "testcase" ()
  | otherwise   = XML.unode "testcase" $ XML.unode "failure" $
                    unlines [ "Property " ++ prop ++ " failed!"
                            , output r
                            ]

formatResults :: [(String,Result)] -> XML.Element
formatResults rs =
  XML.unode "testsuites" $
  XML.unode "testsuite" $
  map (uncurry formatResult) rs

writeResults :: FilePath -> [(String,Result)] -> IO ()
writeResults outputFile rs =
  writeFile outputFile $
  XML.ppTopElement $
  formatResults rs
