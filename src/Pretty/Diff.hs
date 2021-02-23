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
import Data.List (take, transpose)
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
  mconcat
    [ above wrapping multilineContext x y,
      separator separatorText,
      below wrapping multilineContext x y
    ]

-- | Printing The first value and the diff indicator above.
--
--  @
--  Diff.above Diff.NoWrap Diff.FullContext Diff.FullContext "1234" "_23"
--  @
--
--  @
--  ▼ ▼
-- "1234"
--  @
above :: Wrapping -> MultilineContext -> Text -> Text -> Text
above wrapping multilineContext x y =
  let xs = Text.lines x
      ys = Text.lines y
   in sameLength xs ys
        & map ((\(x, y) -> (x, wrap wrapping y)) . above')
        & extractContext multilineContext (False, [], [])
        & Text.unlines
        & Text.dropAround (== '\n')

above' :: (Maybe Text, Maybe Text) -> (Bool, [Text])
above' (Nothing, Just y) =
  (True, withDiffLine First down (if y == "" then [Diff.Second ' '] else map Diff.Second $ Text.unpack y))
above' (Just x, Nothing) =
  (True, withDiffLine First down (if x == "" then [Diff.First ' '] else map Diff.First $ Text.unpack x))
above' (Just x, Just y) =
  let diffs =
        Diff.getDiff
          (Text.unpack x)
          (Text.unpack y)
   in ( any hasDiff diffs,
        withDiffLine First down diffs
      )

-- | Printing The second value and the diff indicator below.
--
--  @
--  Diff.below Diff.NoWrap Diff.FullContext "1234" "_23"
--  @
--
--  @
-- "_23"
--  ▲
--  @
below :: Wrapping -> MultilineContext -> Text -> Text -> Text
below wrapping multilineContext x y =
  let xs = Text.lines x
      ys = Text.lines y
   in sameLength xs ys
        & map ((\(x, y) -> (x, wrap wrapping y)) . below')
        & extractContext multilineContext (False, [], [])
        & Text.unlines
        & Text.dropAround (== '\n')

below' :: (Maybe Text, Maybe Text) -> (Bool, [Text])
below' (Nothing, Just y) =
  (True, withDiffLine Second up (if y == "" then [Diff.Second ' '] else map Diff.Second $ Text.unpack y))
below' (Just x, Nothing) =
  (True, withDiffLine Second up (if x == "" then [Diff.First ' '] else map Diff.First $ Text.unpack x))
below' (Just x, Just y) =
  let diffs =
        Diff.getDiff
          (Text.unpack x)
          (Text.unpack y)
   in ( any hasDiff diffs,
        withDiffLine Second up diffs
      )

wrap :: Wrapping -> [Text] -> Text
wrap wrapping text =
  Text.stripEnd $ case wrapping of
    Wrap n ->
      text
        & fmap (Text.chunksOf n)
        & interleaveLists
        & filter
          ( \x ->
              not (Text.null (Text.dropAround (== ' ') x) && Text.length x >= n)
          )
        & Text.unlines
    NoWrap -> Text.unlines text

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
        First -> filterEmptyLines [Text.pack indicators & Text.stripEnd, Text.pack content & Text.stripEnd]
        Second -> filterEmptyLines [Text.pack content & Text.stripEnd, Text.pack indicators & Text.stripEnd]

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
        else acc ++ take c before ++ [sep]
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
        else take n xs ++ [sep] ++ takeRight n xs
    else
      if length xs <= n
        then xs
        else [sep] ++ takeRight n xs

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
  [ "\n╷\n",
    "│" <> (fromMaybe "" $ ((<>) " ") <$> maybeComparison),
    "\n╵\n"
  ]
    & mconcat

interleaveLists :: [[a]] -> [a]
interleaveLists = mconcat . transpose

filterEmptyLines :: [Text] -> [Text]
filterEmptyLines = filter (not . Text.null . Text.strip)

sameLength :: [a] -> [b] -> [(Maybe a, Maybe b)]
sameLength [] ys = map (\y -> (Nothing, Just y)) ys
sameLength xs [] = map (\x -> (Just x, Nothing)) xs
sameLength (x : xs) (y : ys) = (Just x, Just y) : sameLength xs ys
