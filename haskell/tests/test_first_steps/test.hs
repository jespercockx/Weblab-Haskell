prop_add_and_double_correct :: Int -> Int -> Property
prop_add_and_double_correct x y = add_and_double x y === 2 * (x+y)