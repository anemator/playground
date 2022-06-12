module Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case txts of
      [] -> [paragraph] -- last paragraph
      currentLine : rest ->
        if trim currentLine == ""
          -- concat parsed paragraph with remaining paragraphs
          then paragraph : parseLines [] rest
          -- prepend currentLine to currentParagraph
          else parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words
