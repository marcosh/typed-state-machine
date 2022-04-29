{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphStateMachine.Example.Counter where

import GraphStateMachine.StateMachine

-- singletons-base
import Data.Singletons.Base.TH

$(singletons [d|

  data CounterTag = OnlyCounterTag
    deriving stock (Eq, Show)

  counterTopology :: Topology CounterTag
  counterTopology = MkTopology [(OnlyCounterTag, [OnlyCounterTag])]

  |])

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