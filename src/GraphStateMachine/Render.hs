{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- text
import Data.Text

newtype Graph a = MkGraph [(a, a)]
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

renderMermaid :: Show a => Graph a -> Text
renderMermaid (MkGraph l)
  =  "stateDiagram-v2\n"
  <> foldMap (\(a1, a2) -> pack (show a1) <> " --> " <> pack (show a2) <> "\n") l

class ToValue (a :: k) where
  toValue :: k

class ToListValue (a :: [k]) where
  toListValue :: [k]

instance ToListValue '[] where
  toListValue = []

instance (ToValue a, ToListValue as) => ToListValue (a ': as) where
  toListValue = toValue @_ @a : toListValue @_ @as

class RenderTopology (t :: Topology v) where
  asGraph :: Graph v

instance RenderTopology ('MkTopology '[]) where
  asGraph = MkGraph []

instance (ToValue v, ToListValue vs, RenderTopology ('MkTopology es)) => RenderTopology ('MkTopology ('(v :: k, vs :: [k]) ': (es :: [(k, [k])]))) where
  asGraph = MkGraph ((toValue @_ @v, ) <$> toListValue @_ @vs) <> asGraph @_ @('MkTopology es)
