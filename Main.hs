module Main where

import UI.NCurses

import English
import Input

main = do
  runCurses $ prompt groupFactor (1, 1) 5 7
