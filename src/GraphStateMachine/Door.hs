{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module GraphStateMachine.Door where

import GraphStateMachine.Render
import GraphStateMachine.StateMachine

data DoorTag
  = IsOpen
  | IsClosed
  deriving stock (Eq, Show)

instance ToValue 'IsOpen where
  toValue = IsOpen

instance ToValue 'IsClosed where
  toValue = IsClosed

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

data DoorState (a :: DoorTag) where
  SIsOpen   :: DoorState 'IsOpen
  SIsClosed :: DoorState 'IsClosed

doorMachine :: StateMachine DoorTopology DoorState DoorCommand DoorEvent
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
