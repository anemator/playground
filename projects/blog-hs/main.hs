import System.Directory (doesFileExist)
import System.Environment (getArgs)
import qualified Convert
import qualified Markup
import Html

process :: Title -> String -> String
process title = render . Convert.convert title . Markup.parse

main :: IO ()
main = putStrLn $ process "My page title" "* My header\nMy content"
