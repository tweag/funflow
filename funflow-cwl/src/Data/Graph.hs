{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE StandaloneDeriving  #-}


{-|
Module      : Graph
Description : This module defines a peculiar form of a graph
  and a function to get a pre-order if that graph is a dag.

This module defines a peculiar form of a graph
and a function to get a pre-order if that graph is a dag.

-}


module Data.Graph
  ( getPreOrder
  , GraphExcept (..)
  )
where


import           Control.Monad
    (foldM, (>=>))
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.Typeable
import Control.Exception

import Data.ErrorMonad



-- | $graphdef
type Graph k v = (M.HashMap k v, GetParents k v)
type GetParents k v = v -> ErrM [k]

type Key k = (Ord k, Hashable k, Typeable k, Show k)

-- $graphdef
--
-- One representation of a graph is an
-- adjacency list. Here, we use a completely different
-- representation. Each node in the graph is a key-value
-- pair. So, we can abstractly view the graph as a
-- graph of keys. The edges in the graph are determined by
-- a function that sometimes gets the parents of a node.
-- It takes a node (say 'x :: v') and produces a list of keys
-- that might be valid references to other nodes that point
-- to the given node 'x'. Essentially, we have a reverse
-- adjacency list representation of a graph.


-- | This is an Exception type for anything that
-- could go wrong in getting the linear preorder
-- of a graph with this representation: cycles or
-- the provided 'GetParents' function is faulty.
data GraphExcept k where
  CyclicGraph :: GraphExcept k
  BadNodeRef :: k -> GraphExcept k

deriving instance Key k => Show (GraphExcept k)
deriving instance Key k => Exception (GraphExcept k)




-- | We get a pre-order of graph that's basically
-- a reverse adjacency list by reversing the postorder
getPreOrder :: Key k => Graph k v -> ErrM [[v]]
getPreOrder = fmap reverse . getPostOrder


-- | We use the complement of a standard algorithm
-- for getting the linear pre order of a graph.
-- That algorithm is to mark each node with an
-- in-degree and repeatedly pop a source and
-- decrement that source's neighbors' in-degree.
-- The complement of this algorithm does the same thing
-- but with out-degrees instead of in-degrees and
-- parents instead of neighbors/children.
getPostOrder :: Key k => Graph k v -> ErrM [[v]]
getPostOrder = outDegrees >=> createLinearPostOrder



type OutDegGraph k v = (M.HashMap k (v,Int), GetParents k v)

-- | Assign out degrees to each node.
outDegrees :: Key k => Graph k v -> ErrM (OutDegGraph k v)
outDegrees graph@(hmap, _) =
  updateOutDegrees (M.elems hmap) (mapZeroOutDegree graph)
  where
    mapZeroOutDegree :: Key k => Graph k v -> OutDegGraph k v
    mapZeroOutDegree = tupfst $ M.map (\x -> (x,0))
      where tupfst f (a,b) = (f a, b)

    -- If this works, we have all the output points
    -- Otherwise, some WfNode had bad references to the nodes
    -- that point in to it.
    updateOutDegrees :: Key k =>
      [v] -> OutDegGraph k v -> ErrM (OutDegGraph k v)
    updateOutDegrees pts graph' = foldM updateOnePtOutDegs graph' pts
      where
        -- For the reverse neighbors of each point,
        -- increment the outdegree by one
        -- If the refs to the neighbors are bad, fail.
        updateOnePtOutDegs  :: Key k =>
          OutDegGraph k v -> v -> ErrM (OutDegGraph k v)
        updateOnePtOutDegs graph'' point = do
          let getParents' = snd graph''
          sources <- getParents' point
          foldM incOutDeg graph'' sources




-- | Given an out-degree annotated graph, create a linear
--   post-order. Recall that this uses the opposite
--   of a standard linear pre-order algorithm.
createLinearPostOrder :: forall k v.
  Key k => OutDegGraph k v -> ErrM [[v]]
createLinearPostOrder (pointMap, getParents)
  | M.null pointMap = return []
  | otherwise = do
      let sinks = M.filter isSink pointMap
      let sinksList = map fst $ M.elems sinks
      let withoutSinks = M.difference pointMap sinks
      let cyclicExcept = CyclicGraph :: GraphExcept k
      guardErrM  (not $ M.null sinks) cyclicExcept
      sourcesPerSink <- traverse getParents sinksList
      let allSrcs = concat sourcesPerSink
      smallerGraph <- foldM decOutDeg (withoutSinks,getParents) allSrcs
      recurPostKeyer <- createLinearPostOrder smallerGraph
      return (sinksList:recurPostKeyer)
  where

    isSink :: (t,Int) -> Bool
    isSink (_,0) = True
    isSink _     = False




-- # Graph Operations
type Tweak = Int

-- | Modify the out degree of a given node in a graph.
tweakOutDeg :: Key k =>
  Tweak -> OutDegGraph k v -> k -> ErrM (OutDegGraph k v)
tweakOutDeg tweak (pointMap, getPars) source = do
  guardErrM (M.member source pointMap) (BadNodeRef source)
  return $ (M.adjust tweakPoint source pointMap, getPars)
  where
    tweakPoint :: (t,Int) -> (t,Int)
    tweakPoint (t, i) = (t, i + tweak)

incOutDeg :: Key k => OutDegGraph k v -> k -> ErrM (OutDegGraph k v)
incOutDeg = tweakOutDeg 1

decOutDeg :: Key k => OutDegGraph k v -> k -> ErrM (OutDegGraph k v)
decOutDeg = tweakOutDeg (-1)






