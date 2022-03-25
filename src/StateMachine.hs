{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module StateMachine where

-- base
import Data.Kind (Type)

-- singletons-base
import Data.Singletons.Base.TH

-- see http://marcosh.github.io/post/2021/10/27/ddd-state-machines.html

-- | A `Topology` is a description of the topology of a state machine
--   It contains an `initialPosition` and the collection of allowed transitions
$(singletons [d|

  newtype Topology (vertex :: Type) = MkTopology { edges :: [(vertex, [vertex])] }

  |])

class LookupContains (map :: [(a, [b])]) (key :: a) (value :: b)

instance {-# OVERLAPPING #-} LookupContains ('(a, b : l1) : l2) a b
instance {-# OVERLAPPING #-} LookupContains ('(a, l1) : l2) a b => LookupContains ('(a, x : l1) : l2) a b
instance LookupContains map a b => LookupContains (x : map) a b

-- | We lift at the type level the information contained in the topology
--   An instance of `AllowedTransition topology initial final` means that the `topology` allows transitions from the `initial` to the `final` state
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex)

-- | We create an instance of `AllowedTransition topology initial final` whenever `final` is adjacent to `initial` according to the `topology` of allowed transitions
instance (LookupContains transitions initial final) => AllowedTransition ('MkTopology transitions) initial final

-- State machines

-- | A `StateMachine topology state input output` describes a state machine with state of type `state tag`, input of type `input` and output of type `output`, such that
--   the allowed transition are described by the topology `topology tag`.
--   A state machine is composed by an `initialState` and an `action`, which defines the `output` and the new `state` given the current `state` and an `input`
data StateMachine (topology :: Topology tag) (state :: tag -> Type) (input :: Type) (output :: Type) = MkStateMachine
  { initialState :: InitialState state
  , action       :: forall initialTag. state initialTag -> input -> ActionResult topology state initialTag output
  }

data InitialState (state :: tag -> Type) where
  MkInitialState :: state tag -> InitialState state

-- | The result of an action of the state machine.
--   An `ActionResult topology state initialTag output` contains an `output` and a `state finalTag`, where the transition from `initialTag` to `finalTag` is allowed by the machine `topology`
data ActionResult (topology :: Topology tag) (state :: tag -> Type) (initialTag :: tag) (output :: Type) where
  MkActionResult :: AllowedTransition topology initialTag finalTag => state finalTag -> output -> ActionResult topology state initialTag output

-- A trivial example

data State
  = OnlyValidState

type TrivialTopology = 'MkTopology '[ '( 'OnlyValidState, '[ 'OnlyValidState ] ) ]

data Input = MkInput

data Output = MkOutput

data SState state where
  SOnlyValidState :: SState 'OnlyValidState

constantMachine :: StateMachine TrivialTopology SState Input Output
constantMachine = MkStateMachine
  { initialState = MkInitialState SOnlyValidState
  , action       = \SOnlyValidState _ -> MkActionResult SOnlyValidState MkOutput
  }

-- A slightly less trivial example

data DoorState
  = IsOpen
  | IsClosed

type DoorTopology = 'MkTopology
  '[ '( 'IsClosed, '[ 'IsClosed, 'IsOpen ] )
   , '( 'IsOpen  , '[ 'IsClosed, 'IsOpen ] )
   ]

data DoorCommand
  = Open
  | Close

data DoorEvent
  = NoOp
  | Opened
  | Closed

data SDoorState a where
  SIsOpen   :: SDoorState 'IsOpen
  SIsClosed :: SDoorState 'IsClosed

doorMachine :: StateMachine DoorTopology SDoorState DoorCommand DoorEvent
doorMachine = MkStateMachine
  { initialState = MkInitialState SIsClosed
  , action       =
    \case
      SIsOpen -> \case
        Open  -> MkActionResult SIsOpen   NoOp
        Close -> MkActionResult SIsClosed Closed
      SIsClosed -> \case
        Open  -> MkActionResult SIsOpen   Opened
        Close -> MkActionResult SIsClosed NoOp
  }

-- An even less trivial example

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

-- The same example using singletons

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


-- Things which are not completely satisfactory at the moment:
-- - in the topology I need to manually define that I might not change state
-- - a state machine has a `state` parameter, which might possibly be removed
-- - is there a way to avoid defining `LockDoorState` and `SLockDoorState` when not using singletons?
-- - let inputs and outputs depend on state?
