module Main (main) where

import Lib (mainSimulation)

-- | The entry point for the executable.
-- Delegating directly to the library's simulation logic.
main :: IO ()
main = mainSimulation
