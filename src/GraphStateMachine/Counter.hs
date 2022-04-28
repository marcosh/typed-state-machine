{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module GraphStateMachine.Counter where

import GraphStateMachine.StateMachine

data CounterTag = CounterTag

type CounterTopology = 'MkTopology '[ '( 'CounterTag, '[ 'CounterTag ] ) ]

data Input
  = Increase
  | Decrease

data Output = Output

data CounterState state where
  CounterState :: Int -> CounterState 'CounterTag

counterMachine :: StateMachine CounterTopology CounterState Input Output
counterMachine = MkStateMachine
  { initialState = MkInitialState (CounterState 0)
  , action = \(CounterState i) input -> case input of
      Increase -> MkActionResult (CounterState (i + 1)) Output
      Decrease -> MkActionResult (CounterState (i - 1)) Output
  }