module HsBlog.Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Data.Maybe(maybeToList)
import Numeric.Natural

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

    -- unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock list) ->
          parseLines (Just (CodeBlock (list <> [line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

    -- paragraph case
    currentLine : rest ->
      let line = trim currentLine
      in
      if line == ""
        -- concat parsed paragraph with remaining paragraphs
        then maybe id (:) context (parseLines Nothing rest)
        -- prepend currentLine to currentParagraph
        else case context of-- parseLines (currentLine : currentParagraph) rest
          Just (Paragraph paragraph) ->
            parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
          _ ->
            maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
