{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphStateMachine.SingletonsLockDoor where

import GraphStateMachine.StateMachine

-- singletons-base
import Data.Singletons.Base.TH

$(singletons [d|

  data SingLockDoorState
    = SingIsLockOpen
    | SingIsLockClosed
    | SingIsLockLocked

  singLockDoorTopology :: Topology SingLockDoorState
  singLockDoorTopology = MkTopology
    [ (SingIsLockOpen  , [SingIsLockOpen, SingIsLockClosed])
    , (SingIsLockClosed, [SingIsLockClosed, SingIsLockOpen, SingIsLockLocked])
    , (SingIsLockLocked, [SingIsLockLocked, SingIsLockClosed])
    ]

  |])

data SingLockDoorCommand
  = SingLockOpen
  | SingLockClose
  | SingLockLock
  | SingLockUnlock

data SingLockDoorEvent
  = SingLockNoOp
  | SingLockOpened
  | SingLockClosed
  | SingLockLocked
  | SingLockUnlocked

singLockDoorMachine :: StateMachine SingLockDoorTopology SSingLockDoorState SingLockDoorCommand SingLockDoorEvent
singLockDoorMachine =  MkStateMachine
  { initialState = MkInitialState SSingIsLockClosed
  , action       = \case
      SSingIsLockOpen   -> \case
        SingLockOpen   -> MkActionResult SSingIsLockOpen   SingLockNoOp
        SingLockClose  -> MkActionResult SSingIsLockClosed SingLockClosed
        SingLockLock   -> MkActionResult SSingIsLockOpen   SingLockNoOp
        SingLockUnlock -> MkActionResult SSingIsLockOpen   SingLockNoOp
      SSingIsLockClosed -> \case
        SingLockOpen   -> MkActionResult SSingIsLockOpen   SingLockOpened
        SingLockClose  -> MkActionResult SSingIsLockClosed SingLockNoOp
        SingLockLock   -> MkActionResult SSingIsLockLocked SingLockLocked
        SingLockUnlock -> MkActionResult SSingIsLockClosed SingLockNoOp
      SSingIsLockLocked -> \case
        SingLockOpen   -> MkActionResult SSingIsLockLocked SingLockNoOp
        SingLockClose  -> MkActionResult SSingIsLockLocked SingLockNoOp
        SingLockLock   -> MkActionResult SSingIsLockLocked SingLockNoOp
        SingLockUnlock -> MkActionResult SSingIsLockClosed SingLockUnlocked
  }
