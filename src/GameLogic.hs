-- | Contains game logic
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module GameLogic where

import Control.Applicative
import Control.Lens
import Control.Monad.RWS
import Control.Monad.State
import Data.Array
import Data.Bifunctor
import Data.Bool
import Data.Coerce
import Debug.Trace
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Graphics.Vty
import Linear
import System.Random
import Types
import Utility

startGame :: StdGen -> StdGen -> Board -> GameState
startGame pieceGen colorGen b =
  GameState b 0 newg1 newg2 piece Running initRotationMap MNull
  where
    (shape, newg1) = randomR (0, 6) pieceGen
    (colourNum, newg2) = randomR (0, 6) colorGen
    position = uncurry V2 $ bimap id ((+ 2) . (`div` 2)) $ snd $ bounds b
    piece = makePiece (toEnum shape) position (toEnum colourNum)

clearRows :: V2 Int -> Board -> (Maybe Int, Board)
clearRows (V2 posr posc) b =
  case maxrow of
    Just mr ->
      ( Just numRowsToClear
      , foldr
          (\a -> ixmap ((br, bc), (tr, tc)) (ff a))
          (b // liftA2 (\x y -> ((y, x), Unfilled)) [bc .. tc] rowsToClear)
          rowsToClear)
    Nothing -> (Nothing, b)
  where
    ((br, bc), (tr, tc)) = bounds b
    ff a i
      | fst i >= a = first (min tr . succ) i
      | otherwise = i
    rowRange = [br .. min tr (posr + 2)]
    rowsToClear =
      filter
        (\r -> all (\c -> isFilled $ b ! (r, c)) [bc .. tc])
        rowRange
    numRowsToClear = length rowsToClear
    maxrow =
      if not $ null (rowsToClear)
        then Just $ maximum rowsToClear
        else Nothing
    -- rowRange = [max (posr - 2) br .. min tr (posr + 2)]
      -- filter (\r -> all (\c -> Filled == b ! (r, c)) [bc .. tc]) rowRange

isFilled (Filled _) = True
isFilled _ = False

move :: Movement -> GameState -> GameState
move mv = checksWithFastDown <*> execState go
  where
    go = do
      case mv of
        MRight -> curpiece %= (unsafeMovePieceHorizontally 1)
        MLeft -> curpiece %= (unsafeMovePieceHorizontally (-1))
        MCounterClockwise -> do
          p <- use curpiece
          m <- use rotmap
          let (m',p') = unsafeCounterclockwiseRotation m p
          curpiece .= p'
          rotmap .= m'
        MClockwise -> do
          p <- use curpiece
          m <- use rotmap
          let (m',p') = unsafeClockwiseRotation m p
          curpiece .= p'
          rotmap .= m'
        MDown -> curpiece %= unsafeMovePieceVerticallyDown
        MFastDown -> state $ ((,) ()) . findBottomOFrozenPiece
      movement .= mv
    checksWithFastDown old new =
        case view movement new of
            MFastDown -> freezePiece new
            _ -> checks old new
    checks old new
      | atBottom old new = freezePiece old
      | checkgameover new = set status Done old
      | otherwise =
        case overlaps new old of
          Just (mv, True) ->
            case mv of
              MDown -> freezePiece old
              MNull -> freezePiece old
              _ -> old
          Just (_, False) -> new
          Nothing -> old

findBottomOFrozenPiece =
  ((curpiece . pos . _x) +~ 1) .
  until
    ((||) <$> (not . liftA2 inBoard (views board bounds) actualPiece) <*>
     (evalState go))
    ((curpiece . pos . _x) -~ 1)
  where
    go = do
      ps <- gets actualPiece
      b <- use board
      return (getAny $ foldMap (Any . isFilled . (b !!!)) ps)

(!!!) :: Board -> V2 Int -> Tile
b !!! (V2 i j) = b ! (j,i)


atBottom :: GameState -> GameState -> Bool
atBottom old new =
  let (V2 newposc newposr) = view (curpiece . pos) new
      newcs = map (newposc -) $ new ^.. curpiece . tiles . traverse . _y
      b = fst $ fst $ bounds $ view board old
  in any (< b) newcs

overlaps :: GameState -> GameState -> Maybe (Movement, Bool)
overlaps new old =
  let (V2 posc posr) = view (curpiece . pos) new
      rs = map (posr -) $ new ^.. curpiece . tiles . traverse . _x
      cs = map (posc -) $ new ^.. curpiece . tiles . traverse . _y
      oldboard = view board old
  in if and (zipWith (curry (inRange (bounds oldboard))) cs rs)
       then Just
              ( view movement new
              , (getAny $ foldMap (go . (oldboard !)) (zip cs rs)))
       else Nothing
  where
    go (Filled _) = Any True
    go _ = Any False
       -- then Just (getAny $ foldMap (go . (oldboard !)) (zip cs rs))

freezePiece :: GameState -> GameState
freezePiece g =
  let p = view curpiece g
      colour = view (curpiece . color) g
      cboard = view board g
      position = uncurry V2 $ fmap ((+ 2) . (`div` 2)) $ snd $ bounds $ cboard
      (V2 posc posr) = view (curpiece . pos) g
      rs = g ^.. curpiece . tiles . traverse . _x
      cs = g ^.. curpiece . tiles . traverse . _y
      newboard =
        cboard //
        (zipWith (\r c -> ((posc - c, posr - r), (Filled colour))) rs cs)
      (x, newGen1) = randomR (0, 6) $ view pieceGenerator g
      (c, newGen2) = randomR (0, 6) $ view pieceColourGenerator g
      newpiece = makePiece (toEnum x) position (toEnum c)
      (rr, clearedboard) = clearRows (V2 posc posr) newboard
  in GameState
       clearedboard
       (fromMaybe 0 rr + view rowsCleared g)
       newGen1
       newGen2
       (if checkgameover g
          then set color Types.Background newpiece
          else newpiece)
       (if checkgameover g
          then Done
          else view status g)
       (view rotmap g)
       MNull
      -- newboard = cboard // zipWith (\r c -> ((posc - c, posr - r), Filled)) rs cs

inBoard :: ((Int, Int), (Int, Int)) -> M42 Int -> Bool
inBoard b = getAll . foldMap go
  where
    go = All . inRange b . uncurry (flip (,)) . fromV2


fromV2 (V2 a b) = (a, b)
toV2 (a,b) = V2 a b

-- rs = map (posr -) $ new ^.. curpiece . tiles . traverse . _x
-- cs = map (posc -) $ new ^.. curpiece . tiles . traverse . _y

actualPiece :: GameState -> M42 Int
actualPiece = evalState go
  where
    go = do
      (V2 pc pr) <- use (curpiece . pos)
      ts <- use (curpiece . tiles)
      return (over (mapped . _y) (pc -) (over (mapped . _x) (pr -) ts))

gameover :: GameState -> GameState
gameover = execState go
  where
    go = do
      b <- use board
      let ((_, bc), (tr, tc)) = bounds b
      when
        (or $
         liftA2 (\r c -> isFilled $ b ! (r, c)) [tr, tr - 1, tr - 2] [bc .. tc])
        (status .= Done)

checkgameover = evalState go
  where
    go = do
      b <- use board
      let ((_, bc), (tr, tc)) = bounds b
      return
        (or $
         liftA2 (\r c -> isFilled $ b ! (r, c)) [tr, tr - 1, tr - 2] [bc .. tc])


togglePause :: GameState -> GameState
togglePause g =
  case view status g of
    Paused -> set status Running g
    Running -> set status Paused g
    Done -> g

makeBoard :: Int -> Int -> Either LogicError Board
makeBoard numRows numColumns
  | numRows < 10 || numColumns < 10 = Left BoardError -- can you really have any fun with a smaller board?
  | otherwise =
    Right $ listArray ((0, 0), (numRows - 1, numColumns - 1)) (repeat Unfilled)

defaultBoard :: Board
defaultBoard = listArray ((0, 0), (15, 9)) (repeat Unfilled)

-- makePiece :: Shape -> V2 Int -> PieceColor -> Piece
-- makePiece I = GenericPiece (V4 (V2 0 0) (V2 0 1) (V2 0 2) (V2 0 3)) I
-- makePiece L = GenericPiece (V4 (V2 0 0) (V2 0 1) (V2 0 2) (V2 1 2)) L
-- makePiece J = GenericPiece (V4 (V2 1 0) (V2 1 1) (V2 1 2) (V2 0 2)) J
-- makePiece T = GenericPiece (V4 (V2 0 0) (V2 1 0) (V2 2 0) (V2 1 1)) T
-- makePiece O = GenericPiece (V4 (V2 0 0) (V2 0 1) (V2 1 0) (V2 1 1)) O
-- makePiece S = GenericPiece (V4 (V2 0 1) (V2 1 1) (V2 1 0) (V2 2 0)) S
-- makePiece Z = GenericPiece (V4 (V2 0 0) (V2 1 0) (V2 1 1) (V2 2 1)) Z
makePiece :: Shape -> V2 Int -> PieceColor -> Piece
makePiece I = GenericPiece ((snd $ initRotationMap M.! I) ! 0) I
makePiece L = GenericPiece ((snd $ initRotationMap M.! L) ! 0) L
makePiece J = GenericPiece ((snd $ initRotationMap M.! J) ! 0) J
makePiece T = GenericPiece ((snd $ initRotationMap M.! T) ! 0) T
makePiece O = GenericPiece ((snd $ initRotationMap M.! O) ! 0) O
makePiece S = GenericPiece ((snd $ initRotationMap M.! S) ! 0) S
makePiece Z = GenericPiece ((snd $ initRotationMap M.! Z) ! 0) Z

initRotationMap :: M.Map Shape (Int, Array Int (M42 Int))
initRotationMap =
  M.fromList $
  zipWith
    (\a b -> (a, (0, b)))
    [I .. Z]
    [ishape, lshape, jshape, tshape, oshape, sshape, zshape]
  where
    oshape = listArray (0, 0) [V4 (V2 0 0) (V2 1 0) (V2 0 1) (V2 1 1)]
    lshape =
      listArray
        (0, 3)
        [ (V4 (V2 0 0) (V2 0 1) (V2 0 2) (V2 1 2))
        , (V4 (V2 (-1) 1) (V2 0 1) (V2 1 1) (V2 1 0))
        , (V4 (V2 (-1) 0) (V2 0 0) (V2 0 1) (V2 0 2))
        , (V4 (V2 (-1) 1) (V2 (-1) 2) (V2 0 1) (V2 1 1))
        ]
    jshape =
      listArray
        (0, 3)
        [ (V4 (V2 0 0) (V2 0 1) (V2 0 2) (V2 1 0))
        , (V4 (V2 (-1) 0) (V2 (-1) 1) (V2 0 1) (V2 1 1))
        , (V4 (V2 (-1) 2) (V2 0 0) (V2 0 1) (V2 0 2))
        , (V4 (V2 (-1) 1) (V2 0 1) (V2 1 1) (V2 1 2))
        ]
    sshape =
      listArray
        (0, 1)
        [ (V4 (V2 0 0) (V2 0 1) (V2 1 1) (V2 1 2))
        , (V4 (V2 0 1) (V2 1 0) (V2 1 1) (V2 2 0))
        ]
    zshape =
      listArray
        (0, 1)
        [ (V4 (V2 0 1) (V2 0 2) (V2 1 0) (V2 1 1))
        , (V4 (V2 0 0) (V2 1 0) (V2 1 1) (V2 2 1))
        ]
    tshape =
      listArray
        (0, 3)
        [ (V4 (V2 0 1) (V2 1 0) (V2 1 1) (V2 1 2))
        , (V4 (V2 0 1) (V2 1 1) (V2 1 2) (V2 2 1))
        , (V4 (V2 1 0) (V2 1 1) (V2 1 2) (V2 2 1))
        , (V4 (V2 0 1) (V2 1 0) (V2 1 1) (V2 2 1))
        ]
    ishape =
      listArray
        (0, 1)
        [ (V4 (V2 0 0) (V2 0 1) (V2 0 2) (V2 0 3))
        , (V4 (V2 (-1) 1) (V2 0 1) (V2 1 1) (V2 2 1))
        ]

ccrot :: M22 Int
ccrot = V2 (V2 0 (-1)) (V2 1 0)

crot :: M22 Int
crot = V2 (V2 0 1) (V2 (-1) 0)

unsafeCounterclockwiseRotation = unsafeRotation pred

unsafeClockwiseRotation = unsafeRotation succ

unsafeRotation ::
     (Int -> Int)
  -> M.Map Shape (Int, Array Int (M42 Int))
  -> GenericPiece Int
  -> (M.Map Shape (Int, Array Int (M42 Int)), GenericPiece Int)
unsafeRotation f m p = go (view shape p)
  where
    go =
      \case
        I ->
          let m' = M.update (\(i, xs) -> Just (mod (f i) 2, xs)) I m
          in case (m' M.! I) of
               (i, xs) -> (m', set tiles (xs ! i) p)
        J ->
          let m' = M.update (\(i, xs) -> Just (mod (f i) 4, xs)) J m
          in case (m' M.! J) of
               (i, xs) -> (m', set tiles (xs ! i) p)
        L ->
          let m' = M.update (\(i, xs) -> Just (mod (f i) 4, xs)) L m
          in case (m' M.! L) of
               (i, xs) -> (m', set tiles (xs ! i) p)
        T ->
          let m' = M.update (\(i, xs) -> Just (mod (f i) 4, xs)) T m
          in case (m' M.! T) of
               (i, xs) -> (m', set tiles (xs ! i) p)
        O ->
          let m' = M.update (\(i, xs) -> Just (mod (f i) 1, xs)) O m
          in case (m' M.! O) of
               (i, xs) -> (m', set tiles (xs ! i) p)
        S ->
          let m' = M.update (\(i, xs) -> Just (mod (f i) 2, xs)) S m
          in case (m' M.! S) of
               (i, xs) -> (m', set tiles (xs ! i) p)
        Z ->
          let m' = M.update (\(i, xs) -> Just (mod (f i) 2, xs)) Z m
          in case (m' M.! Z) of
               (i, xs) -> (m', set tiles (xs ! i) p)

-- unsafeCounterClockwiseRotation ::
--      Integral a => GenericPiece a -> GenericPiece Int
-- unsafeCounterClockwiseRotation p = go (view shape p)
--   where
--     go =
--       \case
--         I -> unsafeRotatePiece True $ fmap fromIntegral p
--         L -> unsafeRotatePiece False . fmap fromIntegral $ p
--         J -> unsafeRotatePiece False . fmap fromIntegral $ p
--         T -> unsafeRotatePiece True . fmap fromIntegral $ p
--         O -> fmap fromIntegral p
--         S -> unsafeRotatePiece True . fmap fromIntegral $ p
--         Z -> unsafeRotatePiece True . fmap fromIntegral $ p
-- unsafeRotatePiece False . fmap fromIntegral
-- unsafeClockwiseRotation ::
--      Integral a => GenericPiece a -> GenericPiece Int
-- unsafeClockwiseRotation p = go (view shape p)
--   where
--     go =
--       \case
--         I -> unsafeRotatePiece True . fmap fromIntegral $ p
--         L -> unsafeRotatePiece True . fmap fromIntegral $ p
--         J -> unsafeRotatePiece True . fmap fromIntegral $ p
--         T -> unsafeRotatePiece True . fmap fromIntegral $ p
--         O -> fmap fromIntegral p
--         S -> unsafeRotatePiece True . fmap fromIntegral $ p
--         Z -> unsafeRotatePiece True . fmap fromIntegral $ p
-- -- THIS IS NOT EXPORTED TO THE USER!!
-- unsafeRotatePiece ::
--      Bool
--   -> GenericPiece Int
--   -> GenericPiece Int
-- unsafeRotatePiece = over tiles . (bool (!*! crot) (!*! ccrot))
unsafeMovePieceHorizontally :: Int -> GenericPiece Int -> GenericPiece Int
unsafeMovePieceHorizontally c = over (pos . _y) (+ c)

-- THIS IS NOT EXPORTED TO THE USER!!
unsafeMovePieceVerticallyDown :: GenericPiece Int -> GenericPiece Int
unsafeMovePieceVerticallyDown = over (pos . _x) pred

defaultGameState =
  let gen = mkStdGen 0
  in GameState defaultBoard 0 gen gen (makePiece I (V2 15 7) CWhite) Running
