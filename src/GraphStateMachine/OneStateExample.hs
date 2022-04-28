{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module GraphStateMachine.OneStateExample where

import GraphStateMachine.Render
import GraphStateMachine.StateMachine

data Tag
  = OnlyValidTag
  deriving stock (Eq, Show)

instance ToValue 'OnlyValidTag where
  toValue = OnlyValidTag

type TrivialTopology = 'MkTopology '[ '( 'OnlyValidTag, '[ 'OnlyValidTag ] ) ]

data Input = MkInput

data Output = MkOutput

data SState (state :: Tag) where
  SOnlyValidState :: SState 'OnlyValidTag

constantMachine :: StateMachine TrivialTopology SState Input Output
constantMachine = MkStateMachine
  { initialState = MkInitialState SOnlyValidState
  , action       = \SOnlyValidState _ -> MkActionResult SOnlyValidState MkOutput
  }

