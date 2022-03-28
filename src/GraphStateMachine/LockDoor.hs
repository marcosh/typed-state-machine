{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module GraphStateMachine.LockDoor where

import GraphStateMachine.StateMachine

data LockDoorState
  = IsLockOpen
  | IsLockClosed
  | IsLockLocked

type LockDoorTopology = 'MkTopology
  '[ '( 'IsLockOpen  , '[ 'IsLockOpen  , 'IsLockClosed ])
   , '( 'IsLockClosed, '[ 'IsLockClosed, 'IsLockOpen, 'IsLockLocked ])
   , '( 'IsLockLocked, '[ 'IsLockLocked, 'IsLockClosed ])
   ]

data LockDoorCommand
  = LockOpen
  | LockClose
  | LockLock
  | LockUnlock

data LockDoorEvent
  = LockNoOp
  | LockOpened
  | LockClosed
  | LockLocked
  | LockUnlocked

data SLockDoorState state where
  SIsLockOpen   :: SLockDoorState 'IsLockOpen
  SIsLockClosed :: SLockDoorState 'IsLockClosed
  SIsLockLocked :: SLockDoorState 'IsLockLocked

lockDoorMachine :: StateMachine LockDoorTopology SLockDoorState LockDoorCommand LockDoorEvent
lockDoorMachine = MkStateMachine
  { initialState = MkInitialState SIsLockClosed
  , action       = \case
      SIsLockOpen   -> \case
        LockOpen   -> MkActionResult SIsLockOpen   LockNoOp
        LockClose  -> MkActionResult SIsLockClosed LockClosed
        LockLock   -> MkActionResult SIsLockOpen   LockNoOp
        LockUnlock -> MkActionResult SIsLockOpen   LockNoOp
      SIsLockClosed -> \case
        LockOpen   -> MkActionResult SIsLockOpen   LockOpened
        LockClose  -> MkActionResult SIsLockClosed LockNoOp
        LockLock   -> MkActionResult SIsLockLocked LockLocked
        LockUnlock -> MkActionResult SIsLockClosed LockNoOp
      SIsLockLocked -> \case
        LockOpen   -> MkActionResult SIsLockLocked LockNoOp
        LockClose  -> MkActionResult SIsLockLocked LockNoOp
        LockLock   -> MkActionResult SIsLockLocked LockNoOp
        LockUnlock -> MkActionResult SIsLockClosed LockUnlocked
  }