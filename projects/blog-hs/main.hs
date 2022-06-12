head_ content = "<head>" <> content <> "</head>"
title_ content = "<title>" <> content <> "</title>"
html_ content = "<html>" <> content <> "</html>"
body_ content = "<body>" <> content <> "</body>"

-- <> has right associativity: a <> (b <> c) === a <> b <> c
-- . is function compositiion: (f . g) x === f(g(x))
makeHtml title content = html_ $ (head_ . title_ $ title) <> body_ content

main = putStrLn $ makeHtml "My page title" "My page content"
