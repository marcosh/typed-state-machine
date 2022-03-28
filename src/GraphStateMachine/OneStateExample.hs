{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module GraphStateMachine.OneStateExample where

import GraphStateMachine.StateMachine

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

