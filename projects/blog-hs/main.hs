import System.Directory (doesFileExist)
import System.Environment (getArgs)
import qualified Convert
import qualified Markup
import Html

process :: Title -> String -> String
process title = render . Convert.convert title . Markup.parse

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> putStrLn "Invalid response. use y or n" *>
          confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()

main :: IO ()
-- main = putStrLn $ process "My page title" "* My header\nMy content"
main =
  getArgs >>= \args ->
  case args of
    [] ->
      getContents >>= putStrLn . process "<stdin>" 
    [ifname, ofname] ->
      readFile ifname >>= \contents ->
      doesFileExist ofname >>= \exists ->
      let result = writeFile ofname $ process ifname contents in
      if exists then whenIO confirm result else result
    _ ->
      putStrLn "usage: stack runghc main.hs [<input-file> <output-file>]"
