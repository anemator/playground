import System.Directory (doesFileExist)
import System.Environment (getArgs)
import qualified Convert
import qualified Markup
import Html

process :: Title -> String -> String
process title = render . Convert.convert title . Markup.parse

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  ans <- getLine
  case ans of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()

main :: IO ()
-- main = putStrLn $ process "My page title" "* My header\nMy content"
main = do
  args <- getArgs
  case args of
    [] -> do
      contents <- getContents
      putStrLn $ process "<stdin>" contents
    [ifname, ofname] -> do
      contents <- readFile ifname
      exists <- doesFileExist ofname
      let result = writeFile ofname $ process ifname contents
      if exists then whenIO confirm result else result
    _ ->
      putStrLn "usage: stack runghc main.hs [<input-file> <output-file>]"
