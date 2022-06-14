-- unstable interface to Html (for advanced usage)
module HsBlog.Html.Internal where

import Numeric.Natural

-- left Html: type-name lives in types namespace
-- right Html: constructor lives in the expression namespace
newtype Html = Html String

newtype Structure = Structure String

-- type alias
type Title = String

-- Semigroup is an associative operation that combines things
instance Semigroup Structure where
  (<>) (Structure lhs) (Structure rhs) = Structure $ lhs <> rhs

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar

getStructureString :: Structure -> String
getStructureString (Structure str) = str

-- -> is right associative: a -> b -> c === a -> (b -> c)
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- Monoid satisfies
--   x <> <identity> = x
--   <identity> <> x = x
instance Monoid Structure where
  mempty = Structure ""

p_ :: String -> Structure
p_ = Structure . el "p". escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

ul_ :: [Structure] -> Structure
ul_ xs = Structure $ el "ul" (concat $ map (el "li" . getStructureString) xs)

ol_ :: [Structure] -> Structure
ol_ xs = Structure $ el "ol" (concat $ map (el "li" . getStructureString) xs)

pre_ :: String -> Structure
pre_ = Structure . el "pre" . escape

-- <> has right associativity: a <> (b <> c) === a <> b <> c
-- . is function compositiion: (f . g) x === f(g(x))
html_ :: Title -> Structure -> Html
html_ title content =
  Html $ el "html" $
      (el "head" $ el "title" (escape title))
      <>
      (el "body" $ getStructureString content)

render :: Html -> String
render (Html str) = str
