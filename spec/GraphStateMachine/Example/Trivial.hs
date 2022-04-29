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

module GraphStateMachine.Example.Trivial where

import GraphStateMachine.StateMachine

-- singletons-base
import Data.Singletons.Base.TH

$(singletons [d|

  data OneStateTag
    = OnlyTag
    deriving stock (Eq, Show)

  trivialTopology :: Topology OneStateTag
  trivialTopology = MkTopology [(OnlyTag, [OnlyTag])]

  |])

data Input = MkInput

data Output = MkOutput

constantMachine :: StateMachine TrivialTopology SOneStateTag Input Output
constantMachine = MkStateMachine
  { initialState = MkInitialState SOnlyTag
  , action       = \SOnlyTag _ -> MkActionResult SOnlyTag MkOutput
  }

