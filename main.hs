{-# LANGUAGE RankNTypes #-}
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified UI.HSCurses.Curses as C
import Control.Monad (forever, forM_)
import Control.Applicative
import Data.List (intersperse)

posMove (dx, dy) =
    \(x, y) -> (x + dx, y + dy)

preventMoveIntoWall mf currentPos =
    if inWall then currentPos else newPos
  where
    newPos@(newX, newY) = mf currentPos
    inWall = exampleWorld !! newY !! newX /= Floor

charToMove c = case c of
    'h' -> (-1, 0)
    'j' -> (0, 1)
    'k' -> (0, -1)
    'l' -> (1, 0)
    _   -> (0, 0)

keyToMove k = posMove $ case k of
    (C.KeyChar c) -> charToMove c
    _           -> (0, 0)

data Tile = Floor
          | WallV
          | WallH deriving (Eq)

type World = [[Tile]]

tileToChar t = case t of
    WallH -> '-'
    WallV -> '|'
    Floor -> '.'

width = 20
height = 20

exampleWorld =
    top ++ middle ++ bottom
  where
    top = [(replicate width WallH)]
    row = [WallV] ++ replicate (width-2) Floor ++ [WallV]
    middle = replicate height row
    bottom = top

newtype TopGUI = MkTopGUI GUI

data GUI = TextWindow String
         | StackLayout [GUI]
         | Positioned (Int, Int) GUI

drawPicture (MkTopGUI p) = do
    C.erase
    drawPicture' (0, 0) p
    C.refresh
  where
    drawPicture' (x, y) p = case p of
        (StackLayout ws)     -> forM_ ws $ drawPicture' (x, y)
        (TextWindow s)       -> drawText (x, y) s
        (Positioned pos pic) -> drawPicture' pos pic
    drawText (x, y) s = forM_ (zip [0..] $ lines s) $ \(i, l) ->
        C.mvWAddStr C.stdScr (y+i) x l

renderMap window pos =
    take index rendered ++ "@" ++ drop (index + 1) rendered
  where
    (wx, wy, ww, wh) = window
    (px, py)     = pos
    index        = py * (ww+1) + px
    windowRows   = take wh $ drop wy $ exampleWorld
    renderedRows = map (map tileToChar . take ww . drop wx) windowRows
    rendered     = concat $ intersperse "\n" $ renderedRows

makePicture window pos = MkTopGUI $
    StackLayout [
        TextWindow $ "Position " ++ show pos,
        Positioned (0, 1) $ TextWindow $ renderMap window pos]

main :: IO ()
main = do
    C.initCurses
    C.echo False
    C.cursSet C.CursorInvisible

    (getChAddHandler, getChCallback) <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eKey <- fromAddHandler getChAddHandler

            let eMove    = (preventMoveIntoWall . keyToMove) <$> eKey
                ePos     = accumE (1, 1) eMove
                --eWindow  = accumE (0, 0, 10, 10) (pure $ \x y ->
                ePicture = makePicture (0, 0, 3, 3) <$> ePos

            reactimate (drawPicture <$> ePicture)

    network <- compile networkDescription
    actuate network
    forever $ C.getCh >>= getChCallback

