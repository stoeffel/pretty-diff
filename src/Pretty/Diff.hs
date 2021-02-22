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
    Config (Config, separatorText, wrapping, multilineContext),
    Wrapping (Wrap, NoWrap),
    MultilineContext (FullContext, Surrounding),

    -- * pretty printing
    pretty,
    above,
    below,
  )
where

import qualified Data.Algorithm.Diff as Diff
import Data.Default (Default, def)
import Data.Function ((&))
import Data.List (take, transpose, zipWith)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude

-- | Configuration for `Pretty.Diff.pretty`.
data Config = Config
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
    wrapping :: Wrapping,
    -- | Only used if text passed in is multiline. Options are full or a some surrounding number of lines
    multilineContext :: MultilineContext
  }

instance Default Config where
  def = Config {separatorText = Nothing, wrapping = NoWrap, multilineContext = Surrounding 2 "..."}

-- | Define whether or not to wrap the diffing lines.
data Wrapping
  = Wrap Int
  | NoWrap

-- | Define how much context surrounding diffs you'd like to show.
data MultilineContext
  = FullContext
  | Surrounding Int Text

-- | Printing a full diff of both values separated by some pipes.
pretty :: Config -> Text -> Text -> Text
pretty Config {separatorText, wrapping, multilineContext} x y =
  let xs = Text.lines x
      ys = Text.lines y
   in [ zipWith (above' wrapping) xs ys & extractContext multilineContext (False, [], []) & mconcat,
        separator separatorText,
        zipWith (below' wrapping) xs ys & extractContext multilineContext (False, [], []) & mconcat
      ]
        & mconcat

-- | Printing The first value and the diff indicator above.
--
--  @
--  Diff.above Diff.NoWrap Diff.FullContext "1234" "_23"
--  @
--
--  @
--  ▼ ▼
-- "1234"
--  @
above :: Wrapping -> Text -> Text -> Text
above wrapping x y =
  let (_, res) = above' wrapping x y
   in res

above' :: Wrapping -> Text -> Text -> (Bool, Text)
above' wrapping x y =
  let diffs =
        Diff.getDiff
          (Text.unpack x)
          (Text.unpack y)
   in ( any hasDiff diffs,
        withDiffLine First down diffs
          & wrap wrapping
          & filterEmptyLines
          & Text.unlines
      )

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
  let (_, res) = below' wrapping x y
   in res

below' :: Wrapping -> Text -> Text -> (Bool, Text)
below' wrapping x y =
  let diffs =
        Diff.getDiff
          (Text.unpack x)
          (Text.unpack y)
   in ( any hasDiff diffs,
        withDiffLine Second up diffs
          & wrap wrapping
          & filterEmptyLines
          & Text.unlines
      )

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

extractContext :: MultilineContext -> (Bool, [Text], [Text]) -> [(Bool, Text)] -> [Text]
extractContext FullContext _ xs = map snd xs
extractContext context@(Surrounding c sep) (hadDiff, acc, before) xs =
  case xs of
    [] ->
      if length before <= c
        then acc ++ before
        else acc ++ take c before ++ [sep, "\n"]
    (True, x) : rest ->
      extractContext
        context
        ( True,
          acc ++ splitSurrounding c sep hadDiff before ++ [x],
          []
        )
        rest
    (False, x) : rest ->
      extractContext
        context
        ( hadDiff,
          acc,
          before ++ [x]
        )
        rest

splitSurrounding :: Int -> Text -> Bool -> [Text] -> [Text]
splitSurrounding n sep hadDiff xs =
  if hadDiff
    then
      if length xs <= n * 2
        then xs
        else take n xs ++ [sep, "\n"] ++ takeRight n xs
    else
      if length xs <= n
        then xs
        else [sep, "\n"] ++ takeRight n xs

takeRight :: Int -> [a] -> [a]
takeRight i xs = reverse (take i (reverse xs))

hasDiff :: Diff.Diff Char -> Bool
hasDiff d =
  case d of
    Diff.First _ -> True
    Diff.Second x -> True
    Diff.Both x _ -> False

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
