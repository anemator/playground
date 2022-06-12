-- -> is right associative: a -> b -> c === a -> (b -> c)
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

-- <> has right associativity: a <> (b <> c) === a <> b <> c
-- . is function compositiion: (f . g) x === f(g(x))
makeHtml :: String -> String -> String
makeHtml title content = html_ $ (head_ . title_ $ title) <> body_ content

myHtml :: String
myHtml = makeHtml "My page title" (h1_ "My page header" <> p_ "My page content")

main = putStrLn myHtml
