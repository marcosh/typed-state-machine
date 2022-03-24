{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module StateMachine where

-- base
import Data.Kind (Type)

-- see http://marcosh.github.io/post/2021/10/27/ddd-state-machines.html

-- | A `Topology` is a description of the topology of a state machine
--   It contains an `initialPosition` and the collection of allowed transitions
data Topology (vertex :: Type) = MkTopology
  { initialPosition :: vertex
  , edges :: [(vertex, [vertex])]
  }

-- | The `InitialPosition` class allows to lift the information regarding the initial position to the type level
type family InitialPosition (topology :: Topology vertex) :: vertex where
  InitialPosition ('MkTopology initialPosition edges) = initialPosition

class LookupContains (map :: [(a, [b])]) (key :: a) (value :: b)

instance {-# OVERLAPPING #-} LookupContains ('(a, b : l1) : l2) a b
instance {-# OVERLAPPING #-} LookupContains ('(a, l1) : l2) a b => LookupContains ('(a, x : l1) : l2) a b
instance LookupContains map a b => LookupContains (x : map) a b

-- | We lift at the type level the information contained in the topology
--   An instance of `AllowedTransition topology initial final` means that the `topology` allows transitions from the `initial` to the `final` state
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex)

-- | We create an instance of `AllowedTransition topology initial final` whenever `final` is adjacent to `initial` according to the `topology` of allowed transitions
instance (LookupContains transitions initial final) => AllowedTransition ('MkTopology initialPosition transitions) initial final

-- instance (Maybe_ 'False (ElemSym1 final) (Lookup initial transitions) ~ 'True) => AllowedTransition ('MkTopology initialPosition transitions) initial final

-- State machines

data ActionResult topology state initialTag output where
  MkActionResult :: AllowedTransition topology initialTag finalTag => state finalTag -> output -> ActionResult topology state initialTag output

data StateMachine (topology :: Topology tag) (state :: tag -> Type) (input :: Type) (output :: Type) = MkStateMachine
  { initialState :: state (InitialPosition topology)
  , action       :: forall initialTag. state initialTag -> input -> ActionResult topology state initialTag output
  }

-- A trivial example

data State
  = OnlyValidState

type TrivialTopology = 'MkTopology
  'OnlyValidState
  '[ '( 'OnlyValidState, '[ 'OnlyValidState ] ) ]

data Input = MkInput

data Output = MkOutput

data SState state where
  SOnlyValidState :: SState 'OnlyValidState

constantMachine :: StateMachine TrivialTopology SState Input Output
constantMachine = MkStateMachine
  { initialState = SOnlyValidState
  , action       = \SOnlyValidState _ -> MkActionResult SOnlyValidState MkOutput
  }

-- A slightly less trivial example

data DoorState
  = IsOpen
  | IsClosed

type DoorTopology = 'MkTopology
  'IsClosed
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
  { initialState = SIsClosed
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
  'IsLockClosed
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

door :: StateMachine LockDoorTopology SLockDoorState LockDoorCommand LockDoorEvent
door = MkStateMachine
  { initialState = SIsLockClosed
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
