--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main parses a communicating system and generates various
-- formats among which the corresponding Haskell's data structure
--

import Misc
import CFSM
import DotStuff
import SystemParser
import Data.List as L
import Data.Map.Strict as M
import System.Environment

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then putStr $ usage SYS
            else do
              let (sourcefile, flags) =
                    getCmd SYS progargs
              myPrint flags SYS ("parsing started" ++ "\n" ++ (show flags))
              txt <- readFile sourcefile
              let (_, _, _, ext) =
                    setFileNames sourcefile flags
              flinesIO <- getDotConf
              let flines =
                    M.fromList [(key, flinesIO!key) | key <- M.keys flinesIO]
              let (sys, ptps) = parseSystem ext txt
              let system = 
                    (if (M.member "-sn" flags)
                     then L.map (\cfsm -> (grenameVertex (sn $ statesOf cfsm) cfsm)) sys
                     else sys,
                     ptps
                    )

              let res =
                    case flags!"--fmt" of
                      "hs" -> show system
                      "dot" -> dottifySystem flines system
                      _ -> "Unknown format " ++ (flags!"--fmt")
              putStrLn res
