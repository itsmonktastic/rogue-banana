{-# LANGUAGE RankNTypes #-}
import Reactive.Banana
import Reactive.Banana.Frameworks
import UI.HSCurses.Curses
import System.Time (getClockTime)

main :: IO ()
main = do
   -- initialize your GUI framework
   -- window <- newWindow
   -- ...
   (ah, ahf) <- newAddHandler

   -- describe the event network
   let networkDescription :: forall t. Frameworks t => Moment t ()
       networkDescription = do
           -- input: obtain  Event  from functions that register event
           -- handlers
           --emouse    <- fromAddHandler $ registerMouseEvent window
           --ekeyboard <- fromAddHandler $ registerKeyEvent window
           -- input: obtain  Behavior  from changes
           --btext     <- fromChanges    "" $ registerTextChange editBox
           -- input: obtain  Behavior  from mutable data by polling
           --bdie      <- fromPoll       $ randomRIO (1,6)
           -- only updated when there is input
           eTick <- fromAddHandler ah
           bTime <- fromPoll getClockTime
           eTime <- changes bTime

           -- express event graph
           let
               -- behavior1 = accumB ...
               -- ...
               -- event15 = union event13 event14

           -- output: animate some event occurences
           reactimate $ fmap print eTime
           -- reactimate $ fmap drawCircle eventCircle

   -- compile network description into a network
   network <- compile networkDescription
   -- register handlers and start producing outputs
   putStrLn "compiled"
   actuate network
   putStrLn "actuated"
