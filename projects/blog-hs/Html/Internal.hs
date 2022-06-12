-- unstable interface to Html (for advanced usage)
module Html.Internal where

-- left Html: type-name lives in types namespace
-- right Html: constructor lives in the expression namespace
newtype Html = Html String

newtype Structure = Structure String

-- type alias
type Title = String

append_ :: Structure -> Structure -> Structure
append_ (Structure lhs) (Structure rhs) = Structure $ lhs <> rhs

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

p_ :: String -> Structure
p_ = Structure . el "p". escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

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
