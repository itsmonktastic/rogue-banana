{-# LANGUAGE RankNTypes #-}
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified UI.HSCurses.Curses as C
import Control.Monad (forever, forM_)
import Control.Applicative
import Data.List (intersperse)

posMove (dx, dy) =
    \(x, y) -> (x + dx, y + dy)

-- pre: current pos added to move is in bounds of gmap
preventMoveIntoWall gmap mf currentPos =
    if inWall then currentPos else newPos
  where
    newPos@(newX, newY) = mf currentPos
    inWall = case gmap !! newY !! newX of
        (Floor _) -> False
        _         -> True

charToMove c = case c of
    'h' -> (-1, 0)
    'j' -> (0, 1)
    'k' -> (0, -1)
    'l' -> (1, 0)
    _   -> (0, 0)

keyToMove k = posMove $ case k of
    (C.KeyChar c) -> charToMove c
    _             -> (0, 0)

data Item = Gold Integer deriving (Eq)

data Tile = Floor [Item]
          | WallV
          | WallH deriving (Eq)

type World = [[Tile]]

tileToChar t = case t of
    WallH        -> '-'
    WallV        -> '|'
    Floor []     -> '.'
    Floor (i:is) -> itemToChar i

itemToChar i = case i of
    (Gold _) -> '$'

width = 20
height = 20
wWidth = 15
wHeight = 15


exampleMap =
    top ++ middle ++ bottom
  where
    top = [(replicate width WallH)]
    row = [WallV] ++ replicate (width-3) (Floor []) ++ [Floor [Gold 1], WallV]
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

calcIndex window pos = index
  where
    (wx, wy, ww, wh) = window
    (px, py)     = pos
    index        = (py-wy) * (ww+1) + (px-wx)

renderMap window pos gmap =
    take index rendered ++ "@" ++ drop (index + 1) rendered
  where
    (wx, wy, ww, wh) = window
    (px, py)     = pos
    index        = (py-wy) * ((min ww $ width - wx) + 1) + (px-wx)
    windowRows   = take wh $ drop wy $ gmap
    renderedRows = map (map tileToChar . take ww . drop wx) windowRows
    rendered     = concat $ intersperse "\n" $ renderedRows

makePicture window pos gmap inv = MkTopGUI $
    StackLayout [
        TextWindow $ "Position " ++ show pos ++ ", Window " ++ show window,
        Positioned (0, 1) $ TextWindow $ renderMap window pos gmap,
        Positioned (0, wHeight + 1) $ TextWindow $ "Gold " ++ show inv]

type Window = (Int, Int, Int, Int)
type Position = (Int, Int)

posToWindow :: Position -> Window -> Window
posToWindow pos w
  | scrollBoth  = (wx', wy', ww, wh)
  | scrollHoriz = (wx', wy, ww, wh)
  | scrollVert  = (wx, wy', ww, wh)
  | otherwise   = w
  where
    wx' = max 0 $ px - ww `div` 2
    wy' = max 0 $ py - wh `div` 2
    (px, py)         = pos
    (wx, wy, ww, wh) = w
    scrollHoriz      = px <= wx || px >= wx + ww
    scrollVert       = py <= wy || py >= wy + wh
    scrollBoth       = scrollHoriz && scrollVert

pickup gmap (px, py) = mapping
  where
    tile = gmap !! py !! px
    mapping = case tile of
        (Floor (i:is)) -> \gmap' ->
            let row = gmap' !! py in
            take py gmap' ++
            [take px row ++ [Floor is] ++ drop (px+1) row] ++
            drop (py+1) gmap'
        _              -> id

gold gmap (px, py) = mg
  where
    tile = gmap !! py !! px
    mg = case tile of
        (Floor ((Gold g):is)) -> Just g
        _                     -> Nothing

main :: IO ()
main = do
    C.initCurses
    C.echo False
    C.cursSet C.CursorInvisible

    (getChAddHandler, getChCallback) <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eKey <- fromAddHandler getChAddHandler

            let bMap      = accumB exampleMap ePickup
                bResolve  = preventMoveIntoWall <$> bMap
                eMove     = keyToMove <$> eKey
                eRMove    = bResolve <@> eMove
                ePos      = accumE (1, 1) eRMove
                ePickup   = pickup <$> bMap <@> ePos
                eGold     = filterJust $ gold <$> bMap <@> ePos
                bGold     = accumB 0 $ (+) <$> eGold
                eMkWindow = posToWindow <$> ePos
                bWindow   = accumB (0, 0, wWidth, wHeight) eMkWindow
                bPos      = stepper (1, 1) ePos
                bPicture  = makePicture <$>
                    bWindow <*>
                    bPos <*>
                    bMap <*>
                    bGold

            ePicture <- changes bPicture
            reactimate (drawPicture <$> ePicture)

    network <- compile networkDescription
    actuate network
    forever $ C.getCh >>= getChCallback

