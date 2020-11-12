# Test runner for Haskell tests
This test runner compiles each of the .hs files found in the current
directory, and then runs all the property tests in it using
QuickCheck. Any failures during compilation and counterexamples found
by QuickCheck return a specific string to the student.
