-- | Contains core data-types
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Types where

import Control.Lens
import Control.Monad.RWS
import Data.Array
import qualified Data.Map as M
import Graphics.Vty
import Linear
import System.Random

data KeyboardConfig
  = Qwerty
  | Dvorak
  deriving (Read, Show)

data Shape
  = I
  | L
  | J
  | T
  | O
  | S
  | Z
  deriving (Eq, Ord, Enum, Show)

data MovingPiece

data FrozenPiece

data Movement
  = MLeft
  | MRight
  | MCounterClockwise
  | MClockwise
  | MDown
  | MFastDown
  | MNull
  deriving (Show, Eq)

type Piece = GenericPiece Int

data PieceColor
  = CBlue
  | CWhite
  | CYellow
  | CMagenta
  | CCyan
  | CGreen
  | CRed
  | Background
  deriving (Eq, Enum, Show)

data GenericPiece a = GenericPiece
  { _tiles :: M42 a
  , _shape :: Shape
  , _pos :: V2 Int
  , _color :: PieceColor
  } deriving (Show, Eq, Functor)

makeLenses ''GenericPiece

type Board = Array (Int, Int) Tile

-- type PieceColor = String -- using hex codes is probably better, but that'll do!!!
data Tile
  = Filled PieceColor
  | Unfilled
  | HasMovingPiece
  deriving (Eq, Show)

data Status
  = Running
  | Paused
  | Done
  deriving (Eq, Show)

type RotationMap = M.Map Shape (Int, Array Int (M42 Int))

data GameState = GameState
  { _board :: Board
  , _rowsCleared :: Int
  , _pieceGenerator :: StdGen
  , _pieceColourGenerator :: StdGen
  , _curpiece :: Piece
  , _status :: Status
  , _rotmap :: RotationMap
  , _movement :: Movement
  } deriving (Show)

makeLenses ''GameState

data LogicError
  = BoardError
  | PieceError
  deriving (Show)
