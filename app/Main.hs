module Main where

import qualified WithFree    (someFunc)
import qualified WithoutFree (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  WithFree.someFunc
  WithoutFree.someFunc
