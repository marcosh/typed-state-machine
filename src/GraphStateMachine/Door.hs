{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module GraphStateMachine.Door where

import GraphStateMachine.StateMachine

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
