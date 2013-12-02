{-# LANGUAGE RankNTypes #-}
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified UI.HSCurses.Curses as C
import Control.Monad (forever, forM_)
import Control.Applicative

castEnum = toEnum . fromEnum

posMove (dx, dy) =
    \(x, y) -> (x + dx, y + dy)

preventMoveIntoWall mf currentPos = if inWall then currentPos else newPos
  where
    newPos@(newX, newY) = mf currentPos
    inWall = exampleWorld !! newX !! newY == Wall

charToMove c = case c of
    'h' -> (-1, 0)
    'j' -> (0, 1)
    'k' -> (0, -1)
    'l' -> (1, 0)
    _   -> (0, 0)

keyToMove k = posMove $ case k of
    (C.KeyChar c) -> charToMove c
    _           -> (0, 0)

drawScreen w p = do
    C.erase
    drawWorld w
    drawPlayer p
    C.refresh

drawPlayer (x, y) =
    C.mvAddCh y x (castEnum '@')

drawWorld w = drawWorldRow 0 w
  where
    drawWorldRow ri []     = return ()
    drawWorldRow ri (r:rs) = drawWorldCol ri 0 r rs

    drawWorldCol ri ci [] rs     = drawWorldRow (ri + 1) rs
    drawWorldCol ri ci (c:cs) rs = do
        C.mvAddCh ri ci (castEnum $ tileToChar c)
        drawWorldCol ri (ci + 1) cs rs

data Tile = Floor | Wall deriving (Eq)
type World = [[Tile]]

tileToChar t = case t of
    Wall  -> '#'
    Floor -> '.'

exampleWorld = [
    [Wall, Wall, Wall, Wall, Wall],
    [Wall, Floor, Floor, Floor, Wall],
    [Wall, Floor, Floor, Floor, Wall],
    [Wall, Floor, Floor, Floor, Wall],
    [Wall, Wall, Wall, Wall, Wall]]

main :: IO ()
main = do
    C.initCurses
    C.echo False
    C.cursSet C.CursorInvisible

    (startAddHandler, startCallback) <- newAddHandler
    (getChAddHandler, getChCallback) <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eStart <- fromAddHandler startAddHandler
            eKey   <- fromAddHandler getChAddHandler

            let eMove  = (preventMoveIntoWall . keyToMove) <$> eKey
                bPos   = accumB (2, 2) eMove

            ePos <- changes bPos
            reactimate (drawScreen exampleWorld <$> ePos)

    network <- compile networkDescription
    actuate network
    startCallback ()
    forever $ C.getCh >>= getChCallback

