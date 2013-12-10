module Data.GNG
    ( -- * Types
      Input(len, add, mul)
    , GNG
    , NodeId
    , EdgeId
      -- * Settings
    , GNGSettings
    , def
    , ageMax
    , moveRatio1
    , moveRatio2
    , splitSpan
      -- * Functions
    , initialize
    , modify
    , update
      -- * Outputs
    , getNodes
    , getEdges
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Default
import Data.Foldable (foldr')
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Safe

import Data.GNG.Input
import Data.GNG.Data

type StGNG a = State (GNG a)

initialize :: GNGSettings -> GNG a
initialize s = GNG (Map.empty) (Map.empty) 1 1 s 0

modify :: GNG a -> GNGSettings -> GNG a
modify gng s = gng&settings .~ s

newNodeId :: StGNG a NodeId
newNodeId = newId NodeId nodeSerial

newEdgeId :: StGNG a EdgeId
newEdgeId = newId EdgeId edgeSerial

newId :: (Int -> b) -> Getting Int (GNG a) Int -> StGNG a b
newId c serial = do
    d <- State.gets $ c . (^.serial)
    State.modify (&nodeSerial %~ inc)
    return d
  where
    inc :: Int -> Int
    inc = fromIntegral . (1+) . (fromIntegral::Int -> Integer)

update :: Input a => GNG a -> a -> GNG a
update gng input = State.execState (update' input) gng

update' :: Input a => a -> StGNG a ()
update' input = do
    (n1, n2) <- neighborNodes input
    maybe
        ((^.edgeId) <$> newEdge n1 n2 >> return ())
        (modifyEdge (&age .~ 0))
        $ headMay (intersect (n1^.edgeIds) (n2^.edgeIds))
    moveNodes input n1
    splitNodes
    aging n1

splitNodes :: Input a => StGNG a ()
splitNodes = State.get >>= f
    <$> (+1) . (^.splitPeriod)
    <*> (\a -> a^.settings^.splitSpan)
    <*> (^.nodes)
    <*> (^.edges)
  where
    f sp ss ns es | sp < ss   = return ()
                  | otherwise = do
        State.modify $ (&splitPeriod .~ 0)
        case headMay (intersect (n1^.edgeIds) (n2^.edgeIds)) >>= flip Map.lookup es of
            Nothing -> return ()
            Just e  -> do
                deleteEdges [e^.edgeId]
                deleteEdgeFromNodes e
                new <- newNode newValue
                mapM_ (modifyNode (&moved .~ mh)) $ map (^.nodeId) [new, n1]
                newEdges <- mapM (newEdge new) [n1, n2]
                mapM_ (modifyEdge (&age .~ e^.age)) $ map (^.edgeId) newEdges
      where
        maximumByMoved = maximumBy (\a b -> compare (a^.moved) (b^.moved))
        n1 = maximumByMoved $ Map.elems ns
        n2 = maximumByMoved . getAll ns . map (otherNode n1) . getAll es $ n1^.edgeIds
        mh = n1^.moved / 2
        newValue = mul (add (n1^.value) (n2^.value)) 0.5

moveNodes :: Input a => a -> Node a -> StGNG a ()
moveNodes input node = do
    gng <- State.get
    let (nv, d) = move (node^.value) input (gng^.settings^.moveRatio1)
    modifyNode (\n -> n&value .~ nv &moved %~ (+d)) (node^.nodeId)
    nnids <- neighbors
    forM_ nnids $ modifyNode $ f gng
  where
    neighbors = do
        gng <- State.get
        return
            $ map (otherNode node)
            $ getAll (gng^.edges)
            $ (node^.edgeIds)
    f gng n = n&value .~ v
               &moved %~ (+d)
      where
        (v, d) = move (n^.value) input (gng^.settings^.moveRatio2)

otherNode :: Node a -> Edge -> NodeId
otherNode n e
    | n^.nodeId == e^.left  = e^.right
    | n^.nodeId == e^.right = e^.left
    | otherwise     = error "invalid state"

getAll :: Ord k => Map k v -> [k] -> [v]
getAll m = catMaybes . map (flip Map.lookup m)

aging :: Input a => Node a -> StGNG a ()
aging node = do
    let eids = node^.edgeIds
    forM_ eids $ modifyEdge (&age %~ (+1))
    excludeEdges eids

excludeEdges :: Input a => [EdgeId] -> StGNG a ()
excludeEdges eids = do
    deades <- excludeEdges'
    mapM deleteEdgeFromNodes deades
    deleteIsolatedNodes
  where
    excludeEdges' = do
        gng <- State.get
        let es = gng^.edges
        let deades = filter (\e -> e^.age > gng^.settings^.ageMax)
                $ getAll es eids
        let deadids = map (^.edgeId) deades
        deleteEdges deadids
        return deades
    deleteIsolatedNodes = do
        deadids <- State.gets (Map.keys . Map.filter (\n -> n^.edgeIds == []) . (^.nodes))
        deleteNodes deadids

deleteEdgeFromNodes :: Edge -> StGNG a ()
deleteEdgeFromNodes edge = do
    ns <- State.gets (^.nodes)
    forM_ (map (^.nodeId) $ getAll ns [edge^.left, edge^.right])
        $ modifyNode (&edgeIds %~ filter (/= edge^.edgeId))

deleteNodes :: [NodeId] -> StGNG a ()
deleteNodes nids = do
    gng <- State.get
    State.modify (&nodes .~ foldr' Map.delete (gng^.nodes) nids)

deleteEdges :: [EdgeId] -> StGNG a ()
deleteEdges eids = do
    gng <- State.get
    State.modify (&edges .~ foldl' (flip Map.delete) (gng^.edges) eids)

modifyNode :: (Node a -> Node a) -> NodeId -> StGNG a ()
modifyNode f nid = State.modify (&nodes %~ Map.adjust f nid)

modifyEdge :: (Edge -> Edge) -> EdgeId -> StGNG a ()
modifyEdge f eid = State.modify (&edges %~ Map.adjust f eid)

newEdge :: Node a -> Node a -> StGNG a Edge
newEdge n1 n2 = do
    eid <- newEdgeId
    let nid1 = n1^.nodeId
    let nid2 = n2^.nodeId
    let edge = Edge eid nid1 nid2 0
    State.modify (&edges %~ Map.insert eid edge)
    modifyNode (&edgeIds %~ (eid:)) nid1
    modifyNode (&edgeIds %~ (eid:)) nid2
    return edge

neighborNodes :: Input a => a -> StGNG a (Node a, Node a)
neighborNodes input = do
    gng <- State.get
    f (Map.elems (gng^.nodes)) input nothing nothing
  where
    f _      _ (Nothing, _)    (Just _, _) = error "not reached"
    f []     _ (Just n1, _)    (Just n2, _) = return (n1, n2)
    f []     i m1@(Just _, _)  (Nothing, _) =
        newNode i >>= \n -> f [] i m1 (Just n, infinity)
    f []     i m1@(Nothing, _) _ =
        newNode i >>= \n -> f [] i (Just n, infinity) m1
    f (n:ns) i m1@(_, mx1)     m2@(_, mx2) = if d < mx1
        then f ns i (Just n, d) m1
        else if d < mx2
            then f ns i m1 (Just n, d)
            else f ns i m1 m2
      where
        d = dist i (n^.value)

    nothing = (Nothing, infinity)
    infinity = 1 / 0 :: Double

newNode :: a -> StGNG a (Node a)
newNode i = do
    nid <- newNodeId
    let node = Node nid i [] 0
    State.modify (&nodes %~ Map.insert nid node)
    return node
