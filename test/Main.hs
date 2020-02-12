{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Control.Exception (catch, throw)
import Data.Default (def)
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Pretty.Diff as Diff
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Runners.Reporter as Reporter

main = defaultMainWithIngredients [Reporter.ingredient] tests

tests :: TestTree
tests =
  testGroup
    "Pretty.Diff"
    [ testCase "Comparing two equal strings" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "Hello"
              "Hello"
          )
          [ "Hello" & showText,
            "╷",
            "│",
            "╵",
            "Hello" & showText
          ],
      testCase "Removed characters" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "Hello"
              "Hel"
          )
          [ "   ▼▼" & forEscapedString,
            "Hello" & showText,
            "╷",
            "│",
            "╵",
            "Hel" & showText
          ],
      testCase "Added characters" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "Hel"
              "Hello"
          )
          [ "Hel" & showText,
            "╷",
            "│",
            "╵",
            "Hello" & showText,
            "   ▲▲" & forEscapedString
          ],
      testCase "Changed characters" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "Axxx"
              "Bxxx"
          )
          [ "▼" & forEscapedString,
            "Axxx" & showText,
            "╷",
            "│",
            "╵",
            "Bxxx" & showText,
            "▲" & forEscapedString
          ],
      testCase "Mixed changes" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "12345"
              "1004"
          )
          [ " ▼▼ ▼" & forEscapedString,
            "12345" & showText,
            "╷",
            "│",
            "╵",
            "1004" & showText,
            " ▲▲" & forEscapedString
          ],
      testCase "Multiline changes (on first and last line)" $
        expectDiffToEqual
          ( Diff.pretty
              def
                { Diff.wrapping = Diff.Wrap $ 5 + 1 -- + 1 because of "
                }
              "0900000000"
              "9000000000"
          )
          [ "▼" & forEscapedString,
            "09000" & showTextPrefix,
            "00000" & showTextPostfix,
            "╷",
            "│",
            "╵",
            "90000" & showTextPrefix,
            "00000" & showTextPostfix,
            "    ▲"
          ],
      testCase "Multiline changes (inbetween)" $
        expectDiffToEqual
          ( Diff.pretty
              def
                { Diff.wrapping = Diff.Wrap $ 5 + 1 -- + 1 because of "
                }
              "0000090000"
              "0090000000"
          )
          [ "00000" & showTextPrefix,
            "▼",
            "90000" & showTextPostfix,
            "╷",
            "│",
            "╵",
            "00900" & showTextPrefix,
            "  ▲" & forEscapedString,
            "00000" & showTextPostfix
          ],
      testCase "With separator text" $
        expectDiffToEqual
          ( Diff.pretty
              Diff.Config
                { Diff.separatorText = Just "equals",
                  Diff.wrapping = Diff.Wrap $ 5 + 1 -- + 1 because of "
                }
              "0000090000"
              "0090000000"
          )
          [ "00000" & showTextPrefix,
            "▼",
            "90000" & showTextPostfix,
            "╷",
            "│ equals",
            "╵",
            "00900" & showTextPrefix,
            "  ▲" & forEscapedString,
            "00000" & showTextPostfix
          ]
    ]

expectDiffToEqual actual expected_ = do
  let expected = Text.unlines expected_
  (actual @?= expected)
    `catch` ( \(e :: HUnitFailure) -> do
                Text.IO.putStrLn "Actual was:"
                Text.IO.putStrLn actual
                Text.IO.putStrLn "Expected was:"
                Text.IO.putStrLn expected
                throw e
            )

forEscapedString x = " " <> x

showText = Text.pack . show

showTextPrefix x = "\"" <> x

showTextPostfix x = x <> "\""
