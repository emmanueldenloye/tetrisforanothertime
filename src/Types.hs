-- | Contains core data-types
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module Types where

import Data.Array
import System.Random
import Control.Lens
import Linear
import Graphics.Vty
import Control.Monad.RWS

data KeyboardConfig = Qwerty | Dvorak deriving (Read, Show)

data Shape
  = I
  | L
  | J
  | T
  | O
  | S
  | Z
  deriving (Eq, Enum, Show)

data MovingPiece

data FrozenPiece

data Movement
  = MLeft
  | MRight
  | MCounterClockwise
  | MClockwise
  | MDown

type Piece = GenericPiece Int

data PieceColor = CBlue | CWhite | CYellow | CMagenta | CCyan | CGreen | CRed | Background deriving (Eq, Enum, Show)

data GenericPiece a  = GenericPiece
  { _tiles :: M42 a
  , _shape :: Shape
  , _pos :: V2 Int
  , _color :: PieceColor
  } deriving (Show, Eq, Functor)

makeLenses ''GenericPiece

type Board = Array (Int,Int) Tile

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
  deriving (Show)

data GameState = GameState
  { _board :: Board
  , _rowsCleared :: Int
  , _pieceGenerator :: StdGen
  , _pieceColourGenerator :: StdGen
  , _curpiece :: Piece
  , _status :: Status
  } deriving (Show)

makeLenses ''GameState

data LogicError
  = BoardError
  | PieceError
  deriving (Show)
