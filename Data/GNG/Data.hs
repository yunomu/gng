{-# LANGUAGE TemplateHaskell #-}
module Data.GNG.Data
    where

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map

data NodeId = NodeId Int
  deriving (Eq, Ord, Show)

data EdgeId = EdgeId Int
  deriving (Eq, Ord, Show)

data Node a = Node
    { _nodeId :: NodeId
    , _value :: a
    , _edgeIds :: [EdgeId]
    , _moved :: Double
    }
makeLenses ''Node

data Edge = Edge
    { _edgeId :: EdgeId
    , _left :: NodeId
    , _right :: NodeId
    , _age :: Int
    }
makeLenses ''Edge

data GNGSettings = GNGSettings
    { _ageMax :: Int
    , _moveRatio1 :: Double
    , _moveRatio2 :: Double
    , _splitSpan :: Int
    }
makeLenses ''GNGSettings

instance Default GNGSettings where
    def = GNGSettings 20 0.10 0.05 10

data GNG a = GNG
    { _nodes :: Map NodeId (Node a)
    , _edges :: Map EdgeId Edge
    , _nodeSerial :: Int
    , _edgeSerial :: Int
    , _settings :: GNGSettings
    , _splitPeriod :: Int
    }
makeLenses ''GNG

getNodes :: GNG a -> Map NodeId (a, [EdgeId])
getNodes = Map.map f . (^.nodes)
  where
    f = (,) <$>  (^.value) <*> (^.edgeIds)

getEdges :: GNG a -> Map EdgeId (NodeId, NodeId)
getEdges = Map.map f . (^.edges)
  where
    f = (,) <$> (^.left) <*> (^.right)
