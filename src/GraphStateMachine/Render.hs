{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module GraphStateMachine.Render where

import GraphStateMachine.StateMachine

-- singletons
import Data.Singletons

-- text
import Data.Text

newtype Graph a = MkGraph [(a, a)]
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

renderMermaid :: Show a => Graph a -> Text
renderMermaid (MkGraph l)
  =  "stateDiagram-v2\n"
  <> foldMap (\(a1, a2) -> pack (show a1) <> " --> " <> pack (show a2) <> "\n") l

-- Render a topology

topologyAsGraph :: Topology v -> Graph v
topologyAsGraph (MkTopology edges') = MkGraph $ edges' >>= edgify
  where
    edgify :: (v, [v]) -> [(v, v)]
    edgify (v, vs) = (v, ) <$> vs

-- directly render a graph

stateMachineAsGraph
  :: forall tag topology state input output
  .  (Demote (Topology tag) ~ Topology tag, SingKind tag, SingI topology)
  => StateMachine (topology :: Topology tag) state input output
  -> Graph tag
stateMachineAsGraph _ = topologyAsGraph (demote @topology)
