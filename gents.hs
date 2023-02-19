--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This file contains the main program that generates transition
-- systems our of communicating systems.
--

import SystemParser
import CFSM
import TS
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import Misc
import DotStuff
import System.Environment
import System.Directory
import FSA

main :: IO ()
main =  do
  progargs <- getArgs
  if L.null progargs
    then putStrLn $ usage GENTS
    else do
      let (sourcefile, flags) =
            getCmd GENTS progargs
      let (_, _, _, ext) =
            setFileNames sourcefile flags
      cm <- readFile sourcefile
      flinesIO <- getDotConf
      let flines =
            M.fromList [(key, flinesIO!key) | key <- M.keys flinesIO]
      let (sys, ptps) =
            parseSystem ext cm
      let system =
            (if (M.member "-sn" flags)
             then (L.map (\cfsm -> (grenameVertex (sn $ statesOf cfsm) cfsm)) sys)
             else sys,
             ptps
            )
      let bufferSize =
            read (flags ! "-b") :: Int
      let fifo = not (M.member "-nf" flags)
      let flags' = M.insert "-cp" "" (M.insert "-tp" "- - - -" (M.insert "-p" "" flags))
      let (cs, es, ts) = generate bufferSize fifo system [initConf system] S.empty (S.empty, S.empty, S.empty)
      putStrLn $ ts2String flines "" sourcefile bufferSize fifo system (cs, (initConf system), es, ts) flags' []
      
