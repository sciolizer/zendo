module Main where

import Input

main = do
  runCurses $ prompt groupFactor (1, 1) 5 7
