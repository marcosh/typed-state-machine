{-# LANGUAGE DataKinds #-}
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

-- singletons-base
import Data.List.Singletons (Lookup, ElemSym1)
import Data.Maybe.Singletons (Maybe_)

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

-- | We lift at the type level the information contained in the topology
--   An instance of `AllowedTransition topology initial final` means that the `topology` allows transitions from the `initial` to the `final` state
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex)

-- | We create an instance of `AllowedTransition topology initial final` whenever `final` is adjacent to `initial` according to the `topology` of allowed transitions
instance (Maybe_ 'False (ElemSym1 final) (Lookup initial transitions) ~ 'True) => AllowedTransition ('MkTopology initialPosition transitions) initial final

-- State machines

data ActionResult topology state initialTag output where
  MkActionResult :: AllowedTransition topology initialTag finalTag => state finalTag -> output -> ActionResult topology state initialTag output

data StateMachine (topology :: Topology tag) (state :: tag -> Type) (input :: Type) (output :: Type) = MkStateMachine
  { initialState :: state (InitialPosition topology)
  , action       :: forall initialTag. state initialTag -> input -> ActionResult topology state initialTag output
  }

-- An example

data DoorState
  = IsOpen
  | IsClosed
  | IsLocked

type DoorTopology = 'MkTopology
  'IsClosed
  '[ '( 'IsOpen  , '[ 'IsOpen  , 'IsClosed ])
   , '( 'IsClosed, '[ 'IsClosed, 'IsOpen   ])
   , '( 'IsLocked, '[ 'IsLocked, 'IsClosed ])
   ]

data DoorCommand
  = Open
  | Close
  | Lock
  | Unlock

data DoorEvent
  = NoOp
  | Opened
  | Closed
  | Locked
  | Unlocked

-- can we automate/remove this?
data SDoorState state where
  SIsOpen   :: SDoorState 'IsOpen
  SIsClosed :: SDoorState 'IsClosed
  SIsLocked :: SDoorState 'IsLocked

door :: StateMachine DoorTopology SDoorState DoorCommand DoorEvent
door = MkStateMachine
  { initialState = SIsClosed
  , action       = \case
      SIsOpen   -> \case
        Open   -> MkActionResult SIsOpen NoOp
        Close  -> MkActionResult SIsClosed Closed
        Lock   -> MkActionResult SIsOpen NoOp
        Unlock -> MkActionResult SIsOpen NoOp
      SIsClosed -> \case
        Open   -> MkActionResult SIsOpen Opened
        Close  -> MkActionResult SIsClosed NoOp
        Lock   -> MkActionResult SIsLocked Locked
        Unlock -> MkActionResult SIsClosed NoOp
      SIsLocked -> \case
        Open   -> MkActionResult SIsLocked NoOp
        Close  -> MkActionResult SIsLocked NoOp
        Lock   -> MkActionResult SIsLocked NoOp
        Unlock -> MkActionResult SIsClosed Unlocked
  }
