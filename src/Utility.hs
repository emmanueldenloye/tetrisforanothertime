-- |
module Utility where

import Data.Array
import Types

type ErrorMessage = String

debugArrayIndex :: ErrorMessage -> Board -> (Int, Int) -> Tile
debugArrayIndex err board i =
  let bs = bounds board
  in if inRange bs i
       then board ! i
       else error (err ++ show i ++ " " ++ show (bounds board))

debugArraySet :: ErrorMessage -> Board -> [((Int, Int), Tile)] -> Board
debugArraySet err board ps =
  let bs = bounds board
  in if all (inRange bs . fst) ps
       then board // ps
       else error err
