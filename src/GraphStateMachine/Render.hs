{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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

class DemoteList (a :: [k]) where
  demoteList :: [k]

instance DemoteList '[] where
  demoteList = []

instance (SingKind k, Demote k ~ k, SingI a, DemoteList as) => DemoteList ((a :: k) ': as) where
  demoteList = demote @a : demoteList @_ @as

class RenderTopology (t :: Topology v) where
  asGraph :: Graph v

instance RenderTopology ('MkTopology '[]) where
  asGraph = MkGraph []

instance (SingKind k, Demote k ~ k, SingI v, DemoteList vs, RenderTopology ('MkTopology es)) => RenderTopology ('MkTopology ('(v :: k, vs :: [k]) ': (es :: [(k, [k])]))) where
  asGraph = MkGraph ((demote @v, ) <$> demoteList @_ @vs) <> asGraph @_ @('MkTopology es)
