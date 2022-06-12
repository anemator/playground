-- left Html: type-name lives in types namespace
-- right Html: constructor lives in the expression namespace
newtype Html = Html String
newtype Structure = Structure String

-- type alias
type Title = String

append_ :: Structure -> Structure -> Structure
append_ (Structure lhs) (Structure rhs) = Structure $ lhs <> rhs

getStructureString :: Structure -> String
getStructureString (Structure str) = str

-- -> is right associative: a -> b -> c === a -> (b -> c)
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

-- <> has right associativity: a <> (b <> c) === a <> b <> c
-- . is function compositiion: (f . g) x === f(g(x))
html_ :: Title -> Structure -> Html
html_ title content =
  Html $ el "html" $
      (el "head" $ el "title" title) <> (el "body" $ getStructureString content)

myHtml :: Html
myHtml =
  html_
    "My page title"
    (append_ (h1_ "My page header") (p_ "My page content"))

render :: Html -> String
render (Html str) = str

main = putStrLn $ render myHtml
