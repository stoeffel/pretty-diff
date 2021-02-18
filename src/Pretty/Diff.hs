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
    prettyMultilines,
    above,
    below,
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
pretty :: Config -> Text -> Text -> Text
pretty Config {separatorText, wrapping} x y =
  [ above wrapping x y,
    separator separatorText,
    below wrapping x y
  ]
    & mconcat

-- | Printing a full diff of both values separated by some pipes.
prettyMultilines :: Config -> [Text] -> [Text] -> Text
prettyMultilines Config {separatorText, wrapping} xs ys =
  [ zipWith (\x y -> above wrapping x y) xs ys & mconcat,
    separator separatorText,
    zipWith (\x y -> below wrapping x y) xs ys & mconcat
  ]
    & mconcat

-- | Printing The first value and the diff indicator above.
--
--  @
--  Diff.above Diff.NoWrap "1234" "_23"
--  @
--
--  @
--  ▼ ▼
-- "1234"
--  @
above :: Wrapping -> Text -> Text -> Text
above wrapping x y =
  wrap
    wrapping
    ( Diff.getDiff
        (Text.unpack x)
        (Text.unpack y)
        & withDiffLine First down
    )
    & filterEmptyLines
    & Text.unlines

-- | Printing The second value and the diff indicator below.
--
--  @
--  Diff.below Diff.NoWrap "1234" "_23"
--  @
--
--  @
-- "_23"
--  ▲
--  @
below :: Wrapping -> Text -> Text -> Text
below wrapping x y =
  wrap
    wrapping
    ( Diff.getDiff
        (Text.unpack x)
        (Text.unpack y)
        & withDiffLine Second up
    )
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

withDiffLine :: Position -> Char -> [Diff.Diff Char] -> [Text]
withDiffLine pos differ diffs =
  let (content, indicators) =
        diffs
          & mapMaybe (toDiffLine pos differ)
          & unzip
   in case pos of
        First -> [Text.pack indicators & Text.stripEnd, Text.pack content & Text.stripEnd]
        Second -> [Text.pack content & Text.stripEnd, Text.pack indicators & Text.stripEnd]

toDiffLine :: Position -> Char -> Diff.Diff Char -> Maybe (Char, Char)
toDiffLine pos c d =
  case d of
    Diff.First x -> case pos of
      First -> Just (x, c)
      Second -> Nothing
    Diff.Second x -> case pos of
      First -> Nothing
      Second -> Just (x, c)
    Diff.Both x _ -> Just (x, ' ')

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
