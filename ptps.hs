import Misc
import GCParser
import System.Environment
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = do
  progargs <- getArgs
  if L.null progargs
    then putStrLn $ usage PTPS
    else do
        let ( sourcefile, flags ) =
              getCmd PTPS progargs
        gctxt <- readFile sourcefile
        let ( _, ptps ) =
              case gcgrammar gctxt (0, 0) (0, 0) of
                Ok x -> x
                Er err -> error err
        putStrLn $ S.foldr (\p p' -> if p' =="" then p else p ++ ", " ++ p') "" ptps

