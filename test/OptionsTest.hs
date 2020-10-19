module Main (main) where

import           Data.Version                   (showVersion)
import           System.Process                 (readProcess)

import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit

import           Language.Egison                (version)

main :: IO ()
main = defaultMain . hUnitTestToTests . test $ TestList
    [ TestLabel "load-file option" . TestCase $ assertEgisonCmd
        (interpreter "1\n")
        ["--load-file", "test/fixture/a.egi"]
        "x"
    , TestLabel "test option" . TestCase $ assertEgisonCmd
        "3\n\"This is the third line\"\n"
        ["--test", "test/fixture/b.egi"]
        ""
    , TestLabel "eval option" . TestCase $ assertEgisonCmd
        "[[], [1], [1, 2], [1, 2, 3]]\n"
        ["--eval", "matchAll [1,2,3] as list something with $x ++ _ -> x"]
        ""
    , TestLabel "command option" . TestCase $ assertEgisonCmd
        "1\n"
        ["--command", "print (show 1)"]
        ""
    , TestLabel "TSV option" . TestCase $ assertEgisonCmd
        "2\n3\n5\n7\n11\n13\n17\n19\n23\n29\n"
        ["-T", "-e", "take 10 primes"]
        ""
    , TestLabel "TSV option with tab" . TestCase $ assertEgisonCmd
        "1\t2\t3\n4\t5\n"
        ["-T", "-e", "[[1, 2, 3], [4, 5]]"]
        ""
    , TestLabel "substitute option" . TestCase $ assertEgisonCmd
        "10\n11\n12\n13\n14\n15\n"
        ["--substitute", "\\matchAll as list integer with _ ++ $x :: _ ++ #(x + 5) :: _ -> x"]
        "10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20"
    , TestLabel "map option" . TestCase $ assertEgisonCmd
        "3\n4\n5\n6\n7\n"
        ["--map", "\\x -> x + 2"]
        "1\n2\n3\n4\n5"
    , TestLabel "filter option" . TestCase $ assertEgisonCmd
        "2\n3\n5\n7\n"
        ["--filter", "isPrime"]
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n10"
    , TestLabel "field option" . TestCase $ assertEgisonCmd
        "(10, [2, 5])\n(11, [11])\n(12, [2, 2, 3])\n(13, [13])\n(14, [2, 7])\n(15, [3, 5])\n"
        ["--field", "2c", "-m", "\\x -> x"]
        "10\t2\t5\n11\t11\n12\t2\t2\t3\n13\t13\n14\t2\t7\n15\t3\t5"
    , TestLabel "math option" . TestCase $ assertEgisonCmd
        (interpreter "#latex|\\frac{x}{y}|#\n")
        ["--math", "latex"]
        "x / y"
    , TestLabel "sexpr option" . TestCase $ assertEgisonCmd
        (interpreter "3\n")
        ["--sexpr-syntax"]
        "(+ 1 2)"
    , TestLabel "execute main function" . TestCase $ assertEgisonCmd
        "[\"a\", \"b\", \"c\"]\n"
        ["test/fixture/c.egi", "a", "b", "c"]
        ""
    ]

assertEgisonCmd
  :: String   -- The expected value
  -> [String] -- any arguments for egison command
  -> String   -- standard input for egison command
  -> Assertion
assertEgisonCmd expected args input = do
  actual <- readProcess "stack" ("exec" : "--" : "egison" : args) input
  assertEqual "" expected actual

interpreter :: String -> String
interpreter output = concat
  [ "Egison Version ", showVersion version, "\n"
  , "https://www.egison.org\n"
  , "Welcome to Egison Interpreter!\n"
  , "> ", output
  , "> Leaving Egison Interpreter.\n"
  ]
