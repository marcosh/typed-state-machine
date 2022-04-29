{-# LANGUAGE OverloadedStrings #-}

module GraphStateMachine.RenderSpec where

import GraphStateMachine.Example.Counter
import GraphStateMachine.Example.Door
import GraphStateMachine.Example.LockDoor
import GraphStateMachine.Example.RiskManager
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

    describe "topologyAsGraph" $ do
      it "should render the trivial topology" $ do
        topologyAsGraph trivialTopology `shouldBe` MkGraph
          [ (OnlyTag, OnlyTag)
          ]

      it "should render the counter topology" $ do
        topologyAsGraph counterTopology `shouldBe` MkGraph
          [ (OnlyCounterTag, OnlyCounterTag)
          ]

      it "should render the topology of the Door state machine" $ do
        topologyAsGraph doorTopology `shouldBe` MkGraph
          [ (IsClosed, IsClosed)
          , (IsClosed, IsOpen)
          , (IsOpen  , IsClosed)
          , (IsOpen  , IsOpen)
          ]

      it "should render the topology of the LockDoor state machine" $ do
        topologyAsGraph lockDoorTopology `shouldBe` MkGraph
          [ (IsLockOpen  , IsLockOpen)
          , (IsLockOpen  , IsLockClosed)
          , (IsLockClosed, IsLockClosed)
          , (IsLockClosed, IsLockOpen)
          , (IsLockClosed, IsLockLocked)
          , (IsLockLocked, IsLockLocked)
          , (IsLockLocked, IsLockClosed)
          ]

      it "should render the topology of the RiskManager state machine" $ do
        topologyAsGraph riskTopology `shouldBe` MkGraph
          [ (NoDataTag, NoDataTag)
          , (NoDataTag, CollectedUserDataTag)
          , (CollectedUserDataTag, CollectedUserDataTag)
          , (CollectedUserDataTag, CollectedLoanDetailsFirstTag)
          , (CollectedUserDataTag, ReceivedCreditBureauDataFirstTag)
          , (CollectedLoanDetailsFirstTag, CollectedLoanDetailsFirstTag)
          , (CollectedLoanDetailsFirstTag, CollectedAllDataTag)
          , (ReceivedCreditBureauDataFirstTag, ReceivedCreditBureauDataFirstTag)
          , (ReceivedCreditBureauDataFirstTag, CollectedAllDataTag)
          , (CollectedAllDataTag, CollectedAllDataTag)
          ]

    describe "stateMachineAsGraph" $ do
      it "should render the trivial state machine" $ do
        stateMachineAsGraph constantMachine `shouldBe` MkGraph
          [ (OnlyTag, OnlyTag)
          ]

      it "should render the counter state machine" $ do
        stateMachineAsGraph counterMachine `shouldBe` MkGraph
          [ (OnlyCounterTag, OnlyCounterTag)
          ]

      it "should render the Door state machine" $ do
        stateMachineAsGraph doorMachine `shouldBe` MkGraph
          [ (IsClosed, IsClosed)
          , (IsClosed, IsOpen)
          , (IsOpen  , IsClosed)
          , (IsOpen  , IsOpen)
          ]

      it "should render the LockDoor state machine" $ do
        stateMachineAsGraph lockDoorMachine `shouldBe` MkGraph
          [ (IsLockOpen  , IsLockOpen)
          , (IsLockOpen  , IsLockClosed)
          , (IsLockClosed, IsLockClosed)
          , (IsLockClosed, IsLockOpen)
          , (IsLockClosed, IsLockLocked)
          , (IsLockLocked, IsLockLocked)
          , (IsLockLocked, IsLockClosed)
          ]

      it "should render the RiskManager state machine" $ do
        stateMachineAsGraph riskMachine `shouldBe` MkGraph
          [ (NoDataTag, NoDataTag)
          , (NoDataTag, CollectedUserDataTag)
          , (CollectedUserDataTag, CollectedUserDataTag)
          , (CollectedUserDataTag, CollectedLoanDetailsFirstTag)
          , (CollectedUserDataTag, ReceivedCreditBureauDataFirstTag)
          , (CollectedLoanDetailsFirstTag, CollectedLoanDetailsFirstTag)
          , (CollectedLoanDetailsFirstTag, CollectedAllDataTag)
          , (ReceivedCreditBureauDataFirstTag, ReceivedCreditBureauDataFirstTag)
          , (ReceivedCreditBureauDataFirstTag, CollectedAllDataTag)
          , (CollectedAllDataTag, CollectedAllDataTag)
          ]
