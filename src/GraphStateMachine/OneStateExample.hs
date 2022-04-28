{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module GraphStateMachine.OneStateExample where

import GraphStateMachine.Render
import GraphStateMachine.StateMachine

data State
  = OnlyValidState
  deriving stock (Eq, Show)

instance ToValue 'OnlyValidState where
  toValue = OnlyValidState

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

