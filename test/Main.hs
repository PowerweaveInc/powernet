module Main (main) where

import qualified Test.Data.Network

import           Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain 
     $ testGroup "Test suite" 
     [ Test.Data.Network.tests
     ]