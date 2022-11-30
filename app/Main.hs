module Main where

import Control.Monad (void)
import qualified MyLib as M
import qualified Graphics.T as T

main :: IO ()
main = do
  print "nice"
  T.main
  -- void M.runF
