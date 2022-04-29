{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphStateMachine.Example.LockDoor where

import GraphStateMachine.StateMachine

-- singletons-base
import Data.Singletons.Base.TH

$(singletons [d|

  data LockDoorTag
    = IsLockOpen
    | IsLockClosed
    | IsLockLocked
    deriving stock (Eq, Show)

  lockDoorTopology :: Topology LockDoorTag
  lockDoorTopology = MkTopology
    [ (IsLockOpen  , [IsLockOpen, IsLockClosed])
    , (IsLockClosed, [IsLockClosed, IsLockOpen, IsLockLocked])
    , (IsLockLocked, [IsLockLocked, IsLockClosed])
    ]

  |])

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

singLockDoorMachine :: StateMachine LockDoorTopology SLockDoorTag LockDoorCommand LockDoorEvent
singLockDoorMachine =  MkStateMachine
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
