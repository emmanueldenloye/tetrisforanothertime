-- | Contains game logic

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module GameLogic where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Bool
import Data.Maybe
import Data.Function
import Data.List
import Data.Monoid
import Data.Ord
import Data.Array
import Linear
import System.Random
import Types
import Control.Monad.RWS
import Graphics.Vty
import Data.Coerce
import Data.Foldable
import Debug.Trace
import Data.Bifunctor
import Utility

startGame ::
     StdGen
  -> StdGen
  -> Board
  -> GameState
startGame pieceGen colorGen b =
  let (shape, newg1) = randomR (0, 6) pieceGen
      (colourNum,newg2) = randomR (0,6) colorGen
      position = uncurry V2 $ fmap ((+ 2) . (`div` 2)) $ snd $ bounds b
      piece = makePiece (toEnum shape) position (toEnum colourNum)
  in GameState b 0 newg1 newg2 piece Running

clearRows :: V2 Int -> Board -> (Maybe Int, Board)
clearRows (V2 posr posc) b =
  case maxrow of
    Just mr ->
      ( Just numRowsToClear
      , ixmap
          ((br,bc),(tr,tc))
          -- ((max (mr - numRowsToClear) br, bc), (tr, tc))
          -- (movepieces tr mr)
          (Data.Bifunctor.first (min tr . (+) (1 + mr)))
          (debugArraySet err2 b (liftA2 (\x y -> ((y, x), Unfilled)) [bc .. tc] rowsToClear)))
          -- (b // liftA2 (\x y -> ((y, x), Unfilled)) [bc .. tc] rowsToClear))
    Nothing -> (Nothing, b)
  where
    ((br, bc), (tr, tc)) = bounds b
    rowRange = [br .. min tr (posr + 2)]
    -- rowRange = [max (posr - 2) br .. min tr (posr + 2)]
    rowsToClear =
      filter (\r -> all (\c -> isFilled $ debugArrayIndex err1 b (r, c)) [bc .. tc]) rowRange
      -- filter (\r -> all (\c -> Filled == b ! (r, c)) [bc .. tc]) rowRange
    numRowsToClear = length rowsToClear
    err1 = "at clearRows: at rowsClear"
    err2 = "at clearRows: at maxrow"
    maxrow =
      if not $ null (rowsToClear)
        then Just $ maximum rowsToClear
        else Nothing

isFilled (Filled _) = True
isFilled _ = False

move ::
     Movement
  -> GameState
  -> GameState
move m g =
  let boundary = bounds $ view board g
      newstate =
        case m of
          MRight -> over curpiece (unsafeMovePieceHorizontally 1) g
          MLeft -> over curpiece (unsafeMovePieceHorizontally (-1)) g
          MCounterClockwise -> over curpiece unsafeCounterClockwiseRotation g
          MClockwise -> over curpiece unsafeClockwiseRotation g
          MDown -> over curpiece unsafeMovePieceVerticallyDown g
  in checks g boundary newstate
  where
    checks gg b ns
      | atBottom gg ns = freezePiece gg
      | otherwise =
        case overlaps ns gg of
          Just True -> freezePiece gg
          Just False -> ns
          Nothing -> gg

atBottom :: GameState -> GameState -> Bool
atBottom old new =
  let (V2 newposc newposr) = view (curpiece . pos) new
      newcs = map (newposc -) $ new ^.. curpiece . tiles . traverse . _y
      b = fst $ fst $ bounds $ view board old
  in any (< b) newcs

overlaps :: GameState -> GameState -> Maybe Bool
overlaps new old =
  let (V2 posc posr) = view (curpiece . pos) new
      rs = map (posr -) $ new ^.. curpiece . tiles . traverse . _x
      cs = map (posc -) $ new ^.. curpiece . tiles . traverse . _y
      oldboard = view board old
  in if and (zipWith (curry (inRange (bounds oldboard))) cs rs)
       then Just (getAny $ foldMap (go . debugArrayIndex err oldboard) (zip cs rs))
       -- then Just (getAny $ foldMap (go . (oldboard !)) (zip cs rs))
       else Nothing
  where
    go (Filled _) = Any True
    go _ = Any False
    err = "at overlaps"

freezePiece :: GameState -> GameState
freezePiece g =
  let p = view curpiece g
      colour = view (curpiece . color) g
      cboard = view board g
      position = uncurry V2 $ fmap ((+ 2) . (`div` 2)) $ snd $ bounds $ cboard
      (V2 posc posr) = view (curpiece . pos) g
      rs = g ^.. curpiece . tiles . traverse . _x
      cs = g ^.. curpiece . tiles . traverse . _y
      newboard = debugArraySet errnewboard cboard (zipWith (\r c -> ((posc - c, posr - r), (Filled colour))) rs cs)
      -- newboard = cboard // zipWith (\r c -> ((posc - c, posr - r), Filled)) rs cs
      errnewboard = "at freezePiece"
      (x, newGen1) = randomR (0, 6) $ view pieceGenerator g
      (c, newGen2) = randomR (0, 6) $ view pieceColourGenerator g
      newpiece = makePiece (toEnum x) position (toEnum c)
      (rr, clearedboard) = clearRows (V2 posc posr) newboard
  in GameState
      clearedboard
       (fromMaybe 0 rr + view rowsCleared g)
       newGen1
       newGen2
       newpiece
       (view status g)

inBoard :: ((Int, Int), (Int, Int)) -> M42 Int -> Bool
inBoard b = getAll . foldMap go
  where
    go = All . inRange b . fromV2
    fromV2 (V2 a b) = (a, b)

actualPiece :: GameState -> M42 Int
actualPiece g =
  let (V2 pc pr) = view (curpiece . pos) g
      ts = view (curpiece . tiles) g
  in over (mapped . _y) ((-) pr) (over (mapped . _x) ((-) pc) ts)

gameover :: GameState -> GameState
gameover g =
         let b =  view board g
             ((_, bottomcolumn), (toprow, topcolumn)) = bounds $ b
             cond = or $ liftA2 (\r c -> isFilled $ debugArrayIndex (errgameover ++ " cond") b (r,c)) [toprow, toprow - 1 , toprow - 2] [bottomcolumn .. topcolumn]
             -- cond = or $ liftA2 (\r c -> Filled == b ! (r,c)) [toprow, toprow - 1 , toprow - 2] [bottomcolumn .. topcolumn]
         in if cond then set status Done g else g

checkgameover g = let b =  view board g
                      ((_, bottomcolumn), (toprow, topcolumn)) = bounds $ b
         in or $ liftA2 (\r c -> isFilled $ debugArrayIndex (errgameover ++ " liftA2") b (r,c)) [toprow, toprow - 1 , toprow - 2] [bottomcolumn .. topcolumn]
         -- in or $ liftA2 (\r c -> Filled == b ! (r,c)) [toprow, toprow - 1 , toprow - 2] [bottomcolumn .. topcolumn]

errgameover = "at gameover"

togglePause :: GameState -> GameState
togglePause g =
  case view status g of
    Paused -> set status Running g
    Running -> set status Paused g
    Done -> g

makeBoard :: Int -> Int -> Either LogicError Board
makeBoard numRows numColumns
  | numRows > 10 || numColumns > 10 = Left BoardError -- can you really have any fun with a smaller board?
  | otherwise =
    Right $ listArray ((0, 0), (numRows - 1, numColumns - 1)) (repeat Unfilled)

defaultBoard :: Board
defaultBoard = listArray ((0, 0), (15, 9)) (repeat Unfilled)

makePiece :: Shape -> V2 Int -> PieceColor -> Piece
makePiece I = GenericPiece (V4 (V2 0 0) (V2 0 1) (V2 0 2) (V2 0 3)) I
makePiece L = GenericPiece (V4 (V2 0 0) (V2 0 1) (V2 0 2) (V2 1 2)) L
makePiece J = GenericPiece (V4 (V2 1 0) (V2 1 1) (V2 1 2) (V2 0 2)) J
makePiece T = GenericPiece (V4 (V2 0 0) (V2 1 0) (V2 2 0) (V2 1 1)) T
makePiece O = GenericPiece (V4 (V2 0 0) (V2 0 1) (V2 1 0) (V2 1 1)) O
makePiece S = GenericPiece (V4 (V2 0 1) (V2 1 1) (V2 1 0) (V2 2 0)) S
makePiece Z = GenericPiece (V4 (V2 0 0) (V2 1 0) (V2 1 1) (V2 2 1)) Z

ccrot :: M22 Int
ccrot = V2 (V2 0 (-1)) (V2 1 0)

crot :: M22 Int
crot = V2 (V2 0 1) (V2 (-1) 0)

unsafeCounterClockwiseRotation ::
     Integral a => GenericPiece a -> GenericPiece Int
unsafeCounterClockwiseRotation p = go (view shape p)
  where
    go =
      \case
        I -> unsafeRotatePiece True $ fmap fromIntegral p
        L -> unsafeRotatePiece False . fmap fromIntegral $ p
        J -> unsafeRotatePiece False . fmap fromIntegral $ p
        T -> unsafeRotatePiece True . fmap fromIntegral $ p
        O -> fmap fromIntegral p
        S -> unsafeRotatePiece True . fmap fromIntegral $ p
        Z -> unsafeRotatePiece True . fmap fromIntegral $ p

-- unsafeRotatePiece False . fmap fromIntegral

unsafeClockwiseRotation ::
     Integral a => GenericPiece a -> GenericPiece Int
unsafeClockwiseRotation p = go (view shape p)
  where
    go =
      \case
        I -> unsafeRotatePiece True . fmap fromIntegral $ p
        L -> unsafeRotatePiece True . fmap fromIntegral $ p
        J -> unsafeRotatePiece True . fmap fromIntegral $ p
        T -> unsafeRotatePiece True . fmap fromIntegral $ p
        O -> fmap fromIntegral p
        S -> unsafeRotatePiece True . fmap fromIntegral $ p
        Z -> unsafeRotatePiece True . fmap fromIntegral $ p

-- THIS IS NOT EXPORTED TO THE USER!!
unsafeRotatePiece ::
     Bool
  -> GenericPiece Int
  -> GenericPiece Int
unsafeRotatePiece = over tiles . (bool (!*! crot) (!*! ccrot))

unsafeMovePieceHorizontally ::
     Int
  -> GenericPiece Int
  -> GenericPiece Int
unsafeMovePieceHorizontally c = over (pos . _y) (+ c)

-- THIS IS NOT EXPORTED TO THE USER!!
unsafeMovePieceVerticallyDown :: GenericPiece Int -> GenericPiece Int
unsafeMovePieceVerticallyDown = over (pos . _x) pred

defaultGameState =
  let gen = mkStdGen 0
  in GameState defaultBoard 0 gen gen (makePiece L (V2 15 7) CWhite) Running
