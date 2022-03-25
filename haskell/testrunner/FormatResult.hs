{-# LANGUAGE MultiWayIf #-}

module FormatResult where

import Test.QuickCheck
import qualified Text.XML.Light as XML

nameAttr :: String -> XML.Attr
nameAttr n = XML.Attr (XML.unqual "name") n

weightAttr :: Int -> XML.Attr
weightAttr w = XML.Attr (XML.unqual "weight") (show w)

formatResult :: String -> Int -> Result -> XML.Element
formatResult prop weight r =
  if | isSuccess r -> XML.unode "testcase" attributes
     | otherwise   -> XML.unode "testcase" (attributes, failureNode)
  where
    attributes  = [nameAttr prop , weightAttr weight]
    failureNode =
      XML.unode "failure" $
        unlines [ "Property " ++ prop ++ " failed!"
                , output r
                ]

formatResults :: [(String,Int,Result)] -> XML.Element
formatResults rs =
  XML.unode "testsuites" $
  XML.unode "testsuite" $
  map (\(p,w,r) -> formatResult p w r) rs

writeResults :: FilePath -> [(String,Int,Result)] -> IO ()
writeResults outputFile rs =
  writeFile outputFile $
  XML.ppTopElement $
  formatResults rs
