module Main where

import Gradebook.CLI (parseCommand, run)

main :: IO ()
main = do
  cmd <- parseCommand
  run cmd
