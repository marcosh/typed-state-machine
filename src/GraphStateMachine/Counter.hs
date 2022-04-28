{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module GraphStateMachine.Counter where

import GraphStateMachine.StateMachine

data CounterTag = OnlyCounterTag

type CounterTopology = 'MkTopology '[ '( 'OnlyCounterTag, '[ 'OnlyCounterTag ] ) ]

data Input
  = Increase
  | Decrease

data Output = Output

data CounterState (state :: CounterTag) where
  CounterState :: Int -> CounterState 'OnlyCounterTag

counterMachine :: StateMachine CounterTopology CounterState Input Output
counterMachine = MkStateMachine
  { initialState = MkInitialState (CounterState 0)
  , action = \(CounterState i) input -> case input of
      Increase -> MkActionResult (CounterState (i + 1)) Output
      Decrease -> MkActionResult (CounterState (i - 1)) Output
  }