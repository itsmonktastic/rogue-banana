{-# LANGUAGE RankNTypes #-}
import Reactive.Banana
import Reactive.Banana.Frameworks
import UI.HSCurses.Curses
import Control.Monad (forever)
import Control.Applicative

castEnum = toEnum . fromEnum

posMove (dx, dy) = \(x, y) -> (x + dx, y + dy)

charToMove c = case c of
    'h' -> (-1, 0)
    'j' -> (0, 1)
    'k' -> (0, -1)
    'l' -> (1, 0)
    _   -> (0, 0)

keyToMove k = posMove $ case k of
    (KeyChar c) -> charToMove c
    _           -> (0, 0)

drawScreen (x, y) = do
    erase
    mvAddCh y x (castEnum '@')
    refresh

main :: IO ()
main = do
    initCurses
    echo False
    cursSet CursorInvisible

    (getChHandler, getChCallback) <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eKey  <- fromAddHandler getChHandler
            let eMove = keyToMove <$> eKey
            let bPos  = accumB (0, 0) eMove
            ePos  <- changes bPos
            reactimate (drawScreen <$> ePos)

    network <- compile networkDescription
    actuate network
    forever $ getCh >>= getChCallback

