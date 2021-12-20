import Prelude hiding (reverse)
import qualified Prelude

prop_reverse_nil :: Property
prop_reverse_nil = within 1000000 $ reverse ([] :: [Int]) === []

prop_reverse_cons :: Int -> [Int] -> Property
prop_reverse_cons x xs = within 1000000 $ reverse (x:xs) === reverse xs ++ [x]

prop_reverse_correct :: [Int] -> Property
prop_reverse_correct xs = within 1000000 $ reverse xs === Prelude.reverse xs
