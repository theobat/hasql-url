
module Main
  ( main
  )
where

import           Prelude

import           Test.DocTest

main :: IO ()
main = doctest ["./src"]
-- main = doctest ["-isrc", "src"]
