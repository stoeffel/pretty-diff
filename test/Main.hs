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
          [ "Hello",
            "╷",
            "│",
            "╵",
            "Hello"
          ],
      testCase "Removed characters" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "Hello"
              "Hel"
          )
          [ "   ▼▼",
            "Hello",
            "╷",
            "│",
            "╵",
            "Hel"
          ],
      testCase "Added characters" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "Hel"
              "Hello"
          )
          [ "Hel",
            "╷",
            "│",
            "╵",
            "Hello",
            "   ▲▲"
          ],
      testCase "Changed characters" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "Axxx"
              "Bxxx"
          )
          [ "▼",
            "Axxx",
            "╷",
            "│",
            "╵",
            "Bxxx",
            "▲"
          ],
      testCase "Mixed changes" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "12345"
              "1004"
          )
          [ " ▼▼ ▼",
            "12345",
            "╷",
            "│",
            "╵",
            "1004",
            " ▲▲"
          ],
      testCase "Multiline changes (on first and last line)" $
        expectDiffToEqual
          ( Diff.pretty
              def
                { Diff.wrapping = Diff.Wrap 5,
                  Diff.context = Diff.FullContext
                }
              "0900000000"
              "9000000000"
          )
          [ "▼",
            "09000",
            "00000",
            "╷",
            "│",
            "╵",
            "90000",
            "00000",
            "    ▲"
          ],
      testCase "Multiline changes (inbetween)" $
        expectDiffToEqual
          ( Diff.pretty
              def
                { Diff.wrapping = Diff.Wrap 5,
                  Diff.context = Diff.FullContext
                }
              "0000090000"
              "0090000000"
          )
          [ "00000",
            "▼",
            "90000",
            "╷",
            "│",
            "╵",
            "00900",
            "  ▲",
            "00000"
          ],
      testCase "With separator text" $
        expectDiffToEqual
          ( Diff.pretty
              Diff.Config
                { Diff.separatorText = Just "equals",
                  Diff.wrapping = Diff.Wrap 5,
                  Diff.context = Diff.FullContext
                }
              "0000090000"
              "0090000000"
          )
          [ "00000",
            "▼",
            "90000",
            "╷",
            "│ equals",
            "╵",
            "00900",
            "  ▲",
            "00000"
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
