import Html

myHtml :: Html
myHtml =
  html_
    "My page title"
    (append_ (h1_ "My page header") (p_ "My page content"))

main :: IO ()
main = putStrLn $ render myHtml
