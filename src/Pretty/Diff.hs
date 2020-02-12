{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Printing nice and simple diffs of two values.
--
-- @
-- import qualified Pretty.Diff as Diff
-- import Data.Default (def)
--
-- Diff.pretty def "1234" "_23"
-- @
--
-- Will create a string that looks like this:
--
-- @
--  ▼ ▼
-- "1234"
-- ╷
-- │
-- ╵
-- "_23"
--  ▲
-- @
module Pretty.Diff
  ( -- * Configuration
    Config (Config, separatorText, wrapping),
    Wrapping (Wrap, NoWrap),

    -- * pretty printing
    pretty,
    actual,
    expected,
  )
where

import qualified Data.Algorithm.Diff as Diff
import Data.Default (Default, def)
import Data.Function ((&))
import Data.List (transpose)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Text (Text)
import Prelude

-- | Configuration for `Pretty.Diff.pretty`.
data Config
  = Config
      { -- | Text that gets displayed inbetween the diffed values
        --
        -- @
        -- Diff.pretty def { Diff.separatorText = "differing" } "1234" "_23"
        -- @
        --
        -- Will create a string that looks like this:
        --
        -- @
        --  ▼ ▼
        -- "1234"
        -- ╷
        -- │ differing
        -- ╵
        -- "_23"
        --  ▲
        -- @
        separatorText :: Maybe Text,
        -- | Wrapping text to multiple lines if they are longer than the provided length.
        -- This is useful in combination with [terminal-size](https://hackage.haskell.org/package/terminal-size).
        --
        -- @
        -- Diff.pretty def { Diff.wrapping = Diff.Wrap 6 } "0900000000" "9000000000"
        -- @
        --
        -- Will create a string that looks like this:
        --
        -- @
        --  ▼
        -- "09000
        -- 00000"
        -- ╷
        -- │
        -- ╵
        -- "90000
        -- 00000"
        --     ▲
        -- @
        wrapping :: Wrapping
      }

instance Default Config where
  def = Config {separatorText = Nothing, wrapping = NoWrap}

-- | Define whether or not to wrap the diffing lines.
data Wrapping
  = Wrap Int
  | NoWrap

-- | Printing a full diff of both values separated by some pipes.
pretty :: Show a => Config -> a -> a -> Text
pretty Config {separatorText, wrapping} x y =
  [ actual wrapping x y,
    separator separatorText,
    expected wrapping x y
  ]
    & mconcat

-- | Printing The first value and the diff indicator above.
--
--  @
--  Diff.actual Diff.NoWrap "1234" "_23"
--  @
--
--  @
--  ▼ ▼
-- "1234"
--  @
actual :: Show a => Wrapping -> a -> a -> Text
actual wrapping x y =
  wrap wrapping [diffLine First down x y, Text.pack (show x)]
    & filterEmptyLines
    & Text.unlines

-- | Printing The second value and the diff indicator below.
--
--  @
--  Diff.expected Diff.NoWrap "1234" "_23"
--  @
--
--  @
-- "_23"
--  ▲
--  @
expected :: Show a => Wrapping -> a -> a -> Text
expected wrapping x y =
  wrap wrapping [Text.pack (show y), diffLine Second up x y]
    & filterEmptyLines
    & Text.unlines

wrap :: Wrapping -> [Text] -> [Text]
wrap wrapping text =
  case wrapping of
    Wrap n ->
      text
        & fmap (Text.chunksOf n)
        & interleaveLists
    NoWrap -> text

down :: Char
down = '▼'

up :: Char
up = '▲'

data Position = First | Second

diffLine :: Show a => Position -> Char -> a -> a -> Text.Text
diffLine pos differ a b =
  Diff.getDiff
    (show a)
    (show b)
    & mapMaybe (toDiffLine pos differ)
    & Text.pack
    & Text.stripEnd

toDiffLine :: Position -> Char -> Diff.Diff a -> Maybe Char
toDiffLine pos c d =
  case d of
    Diff.First _ -> case pos of
      First -> Just c
      Second -> Nothing
    Diff.Second _ -> case pos of
      First -> Nothing
      Second -> Just c
    Diff.Both _ _ -> Just ' '

separator :: Maybe Text -> Text
separator maybeComparison =
  [ "╷",
    "│" <> (fromMaybe "" $ ((<>) " ") <$> maybeComparison),
    "╵"
  ]
    & Text.unlines

interleaveLists :: [[a]] -> [a]
interleaveLists = mconcat . transpose

filterEmptyLines :: [Text] -> [Text]
filterEmptyLines = filter (not . Text.null . Text.strip)
