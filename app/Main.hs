module Main where

import Control.Monad (void)
import qualified MyLib as M

main :: IO ()
main = do
  print "nice"
  void M.runF
