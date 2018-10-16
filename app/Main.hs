module Main where

import qualified UI as UI
import System.Environment
import Types

main :: IO ()
main = do
  args <- getArgs
  UI.main (read $ args !! 0) (read $ args !! 1) (read $ args  !! 2)
