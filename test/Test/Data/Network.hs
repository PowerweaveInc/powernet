-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave
-- License     :  MIT
-- Portability :  portable

module Test.Data.Network (tests) where 

import qualified Algebra.Graph.Labelled as Graph

import           Hedgehog             ( (===), property, forAll, tripping )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Data.Network   ( Network, Capacity, node, capacity )
import qualified Data.Network   as Network

import           Test.Tasty           ( TestTree, testGroup )
import           Test.Tasty.Hedgehog  ( testProperty )

tests :: TestTree
tests = testGroup "Powerweave.Network" 
      [ testConversion
      , testPropertyZipWith
      ]


testConversion :: TestTree
testConversion = testProperty "bijection topology/fromGraph" $ property $ do
    eds  <- forAll $ Gen.list (Range.linear (0::Int) 32)  
                    $ (,,) <$> (fmap (capacity . Network.unsafeFinite) ((Gen.integral (Range.linear (0::Int) 1000))))
                           <*> (node <$> Gen.alpha <*> pure ())
                           <*> (node <$> Gen.alpha <*> pure ())
    let graph = Graph.edges eds
    tripping graph Network.fromGraph (Just . Network.topology)


testPropertyZipWith :: TestTree
testPropertyZipWith 
    = testProperty "zipWith" $ property $ do
        eds  <- forAll $ Gen.list (Range.linear 0 32)  
                       $ (,,) <$> (fmap (capacity . Network.unsafeFinite) ((Gen.integral (Range.linear 0 1000))))
                              <*> Gen.alpha
                              <*> Gen.alpha
        let network = edges eds
        (Network.zipWith (,) network network) === (Network.map (\x -> (x, x)) network)
    where
        edges :: Ord a => [(Capacity Int, a, a)] -> Network Int a ()
        edges = Network.overlays . map (\(e, x, y) -> Network.edge e (node x ()) (node y ()))