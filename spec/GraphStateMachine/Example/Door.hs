{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphStateMachine.Example.Door where

import GraphStateMachine.StateMachine

-- singletons-base
import Data.Singletons.Base.TH

$(singletons [d|

  data DoorTag
    = IsOpen
    | IsClosed
    deriving stock (Eq, Show)

  doorTopology :: Topology DoorTag
  doorTopology = MkTopology
    [(IsClosed, [IsClosed, IsOpen])
    ,(IsOpen  , [IsClosed, IsOpen])
    ]

  |])

data DoorCommand
  = Open
  | Close

data DoorEvent
  = NoOp
  | Opened
  | Closed

doorMachine :: StateMachine DoorTopology SDoorTag DoorCommand DoorEvent
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
