{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphStateMachine.StateMachine where

-- base
import Data.Kind (Type)

-- singletons-base
import Data.Singletons.Base.TH

-- see http://marcosh.github.io/post/2021/10/27/ddd-state-machines.html

-- | A `Topology` is a description of the topology of a state machine
--   It contains an `initialPosition` and the collection of allowed transitions
$(singletons [d|

  newtype Topology (vertex :: Type) = MkTopology { edges :: [(vertex, [vertex])] }

  |])

class LookupContains (map :: [(a, [b])]) (key :: a) (value :: b)

instance {-# OVERLAPPING #-} LookupContains ('(a, b : l1) : l2) a b
instance {-# OVERLAPPING #-} LookupContains ('(a, l1) : l2) a b => LookupContains ('(a, x : l1) : l2) a b
instance LookupContains map a b => LookupContains (x : map) a b

-- | We lift at the type level the information contained in the topology
--   An instance of `AllowedTransition topology initial final` means that the `topology` allows transitions from the `initial` to the `final` state
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex)

-- | We create an instance of `AllowedTransition topology initial final` whenever `final` is adjacent to `initial` according to the `topology` of allowed transitions
instance (LookupContains transitions initial final) => AllowedTransition ('MkTopology transitions) initial final

-- State machines

-- | A `StateMachine topology state input output` describes a state machine with state of type `state tag`, input of type `input` and output of type `output`, such that
--   the allowed transition are described by the topology `topology tag`.
--   A state machine is composed by an `initialState` and an `action`, which defines the `output` and the new `state` given the current `state` and an `input`
data StateMachine (topology :: Topology tag) (state :: tag -> Type) (input :: Type) (output :: Type) = MkStateMachine
  { initialState :: InitialState state
  , action       :: forall initialTag. state initialTag -> input -> ActionResult topology state initialTag output
  }

data InitialState (state :: tag -> Type) where
  MkInitialState :: state tag -> InitialState state

-- | The result of an action of the state machine.
--   An `ActionResult topology state initialTag output` contains an `output` and a `state finalTag`, where the transition from `initialTag` to `finalTag` is allowed by the machine `topology`
data ActionResult (topology :: Topology tag) (state :: tag -> Type) (initialTag :: tag) (output :: Type) where
  MkActionResult :: AllowedTransition topology initialTag finalTag => state finalTag -> output -> ActionResult topology state initialTag output
