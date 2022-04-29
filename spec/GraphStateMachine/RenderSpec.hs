{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GraphStateMachine.RenderSpec where

import GraphStateMachine.Example.Counter
import GraphStateMachine.Example.Door
import GraphStateMachine.Example.LockDoor
import GraphStateMachine.Example.Trivial
import GraphStateMachine.Render

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

-- text
import Data.Text as Text

spec :: Spec
spec =
  describe "Render" $ do
    describe "renderMermaid" $ do
      it "should render correctly a graph" $ do
        renderMermaid (MkGraph [(1 :: Int, 1), (1, 2), (1, 3), (2, 3), (3, 1)]) `shouldBe` Text.unlines
          [ "stateDiagram-v2"
          , "1 --> 1"
          , "1 --> 2"
          , "1 --> 3"
          , "2 --> 3"
          , "3 --> 1"
          ]

    describe "RenderTopology" $ do
      it "should render the trivial topology" $ do
        topologyAsGraph @_ @TrivialTopology `shouldBe` MkGraph
          [ (OnlyTag, OnlyTag)
          ]

      it "should render the counter topology" $ do
        topologyAsGraph @_ @CounterTopology `shouldBe` MkGraph
          [ (OnlyCounterTag, OnlyCounterTag)
          ]

      it "should render the topology of the Door state machine" $ do
        topologyAsGraph @_ @DoorTopology `shouldBe` MkGraph
          [ (IsClosed, IsClosed)
          , (IsClosed, IsOpen)
          , (IsOpen  , IsClosed)
          , (IsOpen  , IsOpen)
          ]

      it "should render the topology of the LockDoor state machine" $ do
        topologyAsGraph @_ @LockDoorTopology `shouldBe` MkGraph
          [ (IsLockOpen  , IsLockOpen)
          , (IsLockOpen  , IsLockClosed)
          , (IsLockClosed, IsLockClosed)
          , (IsLockClosed, IsLockOpen)
          , (IsLockClosed, IsLockLocked)
          , (IsLockLocked, IsLockLocked)
          , (IsLockLocked, IsLockClosed)
          ]
