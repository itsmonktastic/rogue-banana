{-# LANGUAGE RankNTypes #-}
import Reactive.Banana
import Reactive.Banana.Frameworks
import UI.HSCurses.Curses
import Control.Applicative

--castEnum = toEnum . fromEnum

posMove (dx, dy) = \(x, y) -> (x + dx, y + dy)

charToMove c = case c of
    'j' -> (1, 0)
    _   -> (0, 0)

keyToMove k = posMove $ case k of
    (KeyChar c) -> charToMove c
    _           -> (0, 0)

main :: IO ()
main = do
    --initScr
    --echo False

    (getChHandler, getChCallback) <- newAddHandler

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eKey  <- fromAddHandler getChHandler
            let eMove = keyToMove <$> eKey
            let bPos  = accumB (0, 0) eMove
            ePos  <- changes bPos
            reactimate (print <$> ePos)

    network <- compile networkDescription
    actuate network
    getChCallback $ KeyChar 'j'
    getChCallback $ KeyChar 'j'
