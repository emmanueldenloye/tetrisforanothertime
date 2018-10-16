{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import GameLogic
import Types
import Data.Array
import Linear
import System.Random
import Numeric.Natural
import Control.Lens
import qualified Graphics.Vty as V
import Debug.Trace
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

data Tick = Tick

defaultApp :: App GameState Tick ()
defaultApp =
  App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEventDvorak
  , appStartEvent = return
  , appAttrMap = const theMap
  }


createApp k =
  App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = case k of Qwerty -> handleEventQwerty; Dvorak -> handleEventDvorak
  , appStartEvent = return
  , appAttrMap = const theMap
  }

main :: Int -> Int -> KeyboardConfig -> IO ()
main r c keyboardConfig = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 1000000
  g <- (`startGame` (either (const (defaultBoard)) id (makeBoard r c))) <$> newStdGen
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) (createApp keyboardConfig) g

ifnotPaused g =
  case view status g of
    Running -> continue . id
    Paused -> continue . const g
    Done -> halt

handleEventQwerty ::
     GameState
  -> BrickEvent () Tick
  -> EventM () (Next GameState)
handleEventQwerty g =
  \case
    (AppEvent Tick) -> ifnotPaused g $ move MDown g
    (VtyEvent (V.EvKey (V.KChar 'j') [])) -> ifnotPaused g $ move MClockwise g
    (VtyEvent (V.EvKey (V.KChar 'f') [])) -> ifnotPaused g $ move MCounterClockwise g
    (VtyEvent (V.EvKey (V.KChar 'd') [])) -> ifnotPaused g $ move MLeft g
    (VtyEvent (V.EvKey (V.KChar 'k') [])) -> ifnotPaused g $ move MRight g
    (VtyEvent (V.EvKey (V.KChar 'o') [])) -> ifnotPaused g $ move MDown g
    (VtyEvent (V.EvKey (V.KChar 'q') [])) -> halt g
    (VtyEvent (V.EvKey (V.KChar 'p') [])) -> continue $ togglePause g
    (VtyEvent (V.EvKey V.KEsc [])) -> halt g
    _ -> ifnotPaused g g

handleEventDvorak ::
     GameState
  -> BrickEvent () Tick
  -> EventM () (Next GameState)
handleEventDvorak g =
  \case
    (AppEvent Tick) -> ifnotPaused g $ move MDown g
    (VtyEvent (V.EvKey (V.KChar 'h') [])) -> ifnotPaused g $ move MClockwise g
    (VtyEvent (V.EvKey (V.KChar 'u') [])) -> ifnotPaused g $ move MCounterClockwise g
    (VtyEvent (V.EvKey (V.KChar 'e') [])) -> ifnotPaused g $ move MLeft g
    (VtyEvent (V.EvKey (V.KChar 't') [])) -> ifnotPaused g $ move MRight g
    (VtyEvent (V.EvKey (V.KChar 'r') [])) -> ifnotPaused g $ move MDown g
    (VtyEvent (V.EvKey (V.KChar 'q') [])) -> halt g
    (VtyEvent (V.EvKey (V.KChar 'p') [])) -> continue (togglePause g)
    (VtyEvent (V.EvKey V.KEsc [])) -> halt g
    _ -> ifnotPaused g g

drawUI :: GameState -> [Widget ()]
drawUI g =
  [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: GameState -> Widget ()
drawStats g =
  hLimit 11 $
  vBox [drawScore (g ^. rowsCleared), padTop (Pad 2) $ drawGameOver (checkgameover g)]

drawScore :: Int -> Widget ()
drawScore n =
  withBorderStyle BS.unicodeBold $
  B.borderWithLabel (str "Score") $ C.hCenter $ padAll 1 $ str $ show n

drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget
  where
    gameOverAttr = "gameOver"

drawGrid :: GameState -> Widget ()
drawGrid gg =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Tetris") $ vBox rows
  where
    g = updateMovingPiece gg
    rows = [hBox $ tilesInRow r | r <- [height , height - 1 ..  0]]
    tilesInRow y = [drawCoord (y, x) | x <- [0 .. width]]
    (height, width) = snd $ bounds $ view board g
    drawCoord = drawTile g . tileAt
    tileAt = (view board g !)

updateMovingPiece :: GameState -> GameState
updateMovingPiece g =
  let b = view board g
      p = view curpiece g
      position = view pos p
  in set board (drawPiece (preRefreshPiece b) p position) g
  where
    drawPiece b p (V2 posc posr) =
      let rs = p ^.. tiles . traverse . _x
          cs = p ^.. tiles . traverse . _y
      in b // (zipWith (\r c -> ((posc - c, posr - r), HasMovingPiece)) rs cs)

preRefreshPiece :: Board -> Board
preRefreshPiece = fmap go
  where
    go =
      \case
        HasMovingPiece -> Unfilled
        a@_ -> a

drawTile :: GameState -> Tile -> Widget ()
drawTile g =
  \case
    Filled -> withAttr (currentpieceAttr (view (curpiece . shape) g)) (str " ")
    Unfilled -> withAttr backgroundAttr (str " ")
    HasMovingPiece ->
      withAttr (currentpieceAttr (view (curpiece . shape) g)) (str " ")

currentpieceAttr =
  \case
    I -> iattr
    L -> lattr
    J -> jattr
    T -> tattr
    O -> oattr
    S -> sattr
    Z -> zattr

iattr, lattr, jattr, tattr, oattr, sattr, zattr :: AttrName
backgroundAttr :: AttrName

iattr = "iattr"
lattr = "lattr"
jattr = "jattr"
tattr = "tattr"
oattr = "oattr"
sattr = "sattr"
zattr = "zattr"


backgroundAttr = "backgroundAttr"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (iattr, V.blue `on` V.blue)
    , (lattr, V.white `on` V.white)
    , (jattr, V.yellow `on` V.yellow)
    , (tattr, V.magenta `on` V.magenta)
    , (oattr, V.cyan `on` V.cyan)
    , (sattr, V.green `on` V.green)
    , (zattr, V.red `on` V.red)
    , (backgroundAttr, V.black `on` V.black)
    ]
