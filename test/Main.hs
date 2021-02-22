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
    [ testGroup
        "pretty"
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
                  def {Diff.wrapping = Diff.Wrap 5}
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
                    { Diff.wrapping = Diff.Wrap 5
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
                  def
                    { Diff.separatorText = Just "equals",
                      Diff.wrapping = Diff.Wrap 5
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
        ],
      testGroup
        "pretty (multiline)"
        [ testCase "Multiline content with full multiline context" $
            expectDiffToEqual
              ( Diff.pretty
                  def {Diff.multilineContext = Diff.FullContext}
                  "a\nb\nc\nd\ne\nf"
                  "a\nb\nc\nD\ne\nf"
              )
              [ "a",
                "b",
                "c",
                "▼",
                "d",
                "e",
                "f",
                "╷",
                "│",
                "╵",
                "a",
                "b",
                "c",
                "D",
                "▲",
                "e",
                "f"
              ]
        ],
      testCase "Multiline content with default multiline config" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "a\nb\nc\nd\ne\nf"
              "a\nb\nc\nD\ne\nf"
          )
          [ "...",
            "b",
            "c",
            "▼",
            "d",
            "e",
            "f",
            "╷",
            "│",
            "╵",
            "...",
            "b",
            "c",
            "D",
            "▲",
            "e",
            "f"
          ],
      testCase "Multiline content with narrow multiline context" $
        expectDiffToEqual
          ( Diff.pretty
              def {Diff.multilineContext = Diff.Surrounding 1 "..."}
              "a\nb\nc\nd\ne\nf"
              "a\nb\nc\nD\ne\nf"
          )
          [ "...",
            "c",
            "▼",
            "d",
            "e",
            "...",
            "╷",
            "│",
            "╵",
            "...",
            "c",
            "D",
            "▲",
            "e",
            "..."
          ],
      testCase "Multiline content with narrow context and multiple diffs" $
        expectDiffToEqual
          ( Diff.pretty
              def {Diff.wrapping = Diff.Wrap 3, Diff.multilineContext = Diff.Surrounding 1 "..."}
              "a\nb\nc\nd\ne\nf\ng\nh\ni\nj"
              "A\nb\nc\nD\ne\nf\ng\nh\ni\nJJJJJJ"
          )
          [ "▼",
            "a",
            "b",
            "c",
            "▼",
            "d",
            "e",
            "...",
            "i",
            "▼",
            "j",
            "╷",
            "│",
            "╵",
            "A",
            "▲",
            "b",
            "c",
            "D",
            "▲",
            "e",
            "...",
            "i",
            "JJJ",
            "▲▲▲",
            "JJJ",
            "▲▲▲"
          ],
      testCase "addition in actual" $
        expectDiffToEqual
          ( Diff.pretty
              def {Diff.wrapping = Diff.Wrap 3, Diff.multilineContext = Diff.Surrounding 1 "..."}
              "a\nb\nc\nxd\ne\n"
              "a\nb\nc\nd\ne\n"
          )
          [ "...",
            "c",
            "▼",
            "xd",
            "e",
            "╷",
            "│",
            "╵",
            "...",
            "c",
            "d",
            "e"
          ],
      testCase "wrapping w/ surrounding multilineContext" $
        expectDiffToEqual
          ( Diff.pretty
              def {Diff.wrapping = Diff.Wrap 3, Diff.multilineContext = Diff.Surrounding 1 "..."}
              "a\nb\nc\nxdddddddd\ne\n"
              "a\nb\nc\nddddddddd\ne\n"
          )
          [ "...",
            "c",
            "▼",
            "xdd",
            "ddd",
            "ddd",
            "e",
            "╷",
            "│",
            "╵",
            "...",
            "c",
            "ddd",
            "ddd",
            "ddd",
            "  ▲",
            "e"
          ],
      testCase "few lines (no diff)" $
        expectDiffToEqual
          ( Diff.pretty
              def {Diff.wrapping = Diff.NoWrap, Diff.multilineContext = Diff.Surrounding 1 "..."}
              "a\n"
              "a\n"
          )
          [ "a",
            "╷",
            "│",
            "╵",
            "a"
          ],
      testCase "first is longer" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "a\nB"
              "a\n"
          )
          [ "a",
            "▼",
            "B",
            "╷",
            "│",
            "╵",
            "a"
          ],
      testCase "second is longer" $
        expectDiffToEqual
          ( Diff.pretty
              def
              "a\n"
              "a\nB"
          )
          [ "a",
            "╷",
            "│",
            "╵",
            "a",
            "B",
            "▲"
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
