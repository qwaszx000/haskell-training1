{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Category (Category)
import Control.Monad (when)
import Data.IORef

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import FRP.Yampa
import GHC.Float (double2Float)

-- https://github.com/gelisam/frp-zoo/blob/master/Yampa-example/Main.hs

main :: IO ()
main =
    playYampa
        dmode
        bgColor
        20
        mainSF

instance (Semigroup b) => Semigroup (SF a b) where
    -- (<>) :: Semigroup b => SF a b -> SF a b -> SF a b
    sf1 <> sf2 = (sf1 &&& sf2) >>> arr (uncurry (<>))

instance (Monoid b) => Monoid (SF a b) where
    -- mempty :: Monoid b => SF a b
    mempty = constant mempty

mainSF :: SF (Event G.Event) Picture
mainSF = displayPlayer <> displayCursor

playYampa :: Display -> Color -> Int -> SF (Event G.Event) Picture -> IO ()
playYampa dis col freq hand = do
    pic <- newIORef Blank

    rhand <-
        reactInit
            (pure NoEvent)
            ( \_ changed p -> do
                when changed $ writeIORef pic p
                pure False
            )
            hand

    G.playIO
        dis
        col
        freq
        ()
        (const $ readIORef pic)
        ( \ev _ -> do
            _ <- react rhand (0, Just $ Event ev)
            pure ()
        )
        ( \dt _ -> do
            -- Nothing instead of Just NoEvent makes it to repeat previous event each step
            -- So if you hold some key - you'll send this event on each step
            -- By doing Just NoEvent we are sending NoEvent on step, and events are sent only when they happen
            _ <- react rhand (realToFrac dt, Just NoEvent)
            pure ()
        )

-- Seems like we can use 0 now, so no need for this stuff
-- where
--   eventInterval :: Double
--   eventInterval = 0.001 / fromIntegral freq

dmode :: Display
dmode = InWindow "My title" (200, 200) (20, 20)

bgColor :: Color
bgColor = white

mousePos :: SF (Event G.Event) (Float, Float)
mousePos = delayEvent 2 >>> arrEPrim getMousePos >>> hold (0, 0)
  where
    getMousePos :: Event G.Event -> Event (Float, Float)
    getMousePos (Event (G.EventMotion pos)) = Event pos
    getMousePos _ = NoEvent

cursorPict :: SF (Float, Float) Picture
cursorPict = arrPrim drawCursor
  where
    drawCursor :: (Float, Float) -> Picture
    drawCursor (x, y) = translate x y $ color (makeColor 0.5 0.5 0 0.6) $ circleSolid cursorSize

    cursorSize :: Float
    cursorSize = 10

displayCursor :: SF (Event G.Event) Picture
displayCursor = mousePos >>> cursorPict

data PlayerActions = PlayerJump

data Player = Player {pl'Pos :: (Float, Float), pl'JumpsC :: Int}

initPlayer :: Player
initPlayer = Player (0, 0) 0

playerEvents :: SF (Event G.Event) (Event PlayerActions)
playerEvents = arrEPrim $ mapFilterE selectJumps
  where
    selectJumps :: G.Event -> Maybe PlayerActions
    selectJumps (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) = Just PlayerJump
    selectJumps _ = Nothing

playerAct :: SF (Event PlayerActions) Player
playerAct =
    loopPre
        initPlayer
        $ processPlayer >>> (identity &&& identity)
  where
    processPlayer :: SF (Event PlayerActions, Player) Player
    processPlayer = proc (act, pl) -> do
        fact <- playerFilterActions -< attach act pl
        pl1 <- arrPrim processPlayerActions -< (fact, pl)
        pl2 <- processPlayerGravity -< pl1
        returnA -< pl2

    processPlayerActions :: (Event PlayerActions, Player) -> Player
    processPlayerActions (Event PlayerJump, pl@Player{..}) =
        pl
            { pl'JumpsC = (pl'JumpsC + 1) `mod` 10
            , pl'Pos = second (+ 10) pl'Pos
            }
    processPlayerActions (_, pl) = pl

    processPlayerGravity :: SF Player Player
    processPlayerGravity =
        (identity &&& arr pl'Pos)
            >>> second objGravity
            >>> second (second $ arr (max 0))
            >>> arr (\(pl, pos) -> pl{pl'Pos = pos})

playerFilterActions :: SF (Event (PlayerActions, Player)) (Event PlayerActions)
playerFilterActions = arrEPrim $ mapFilterE filterActions
  where
    filterActions :: (PlayerActions, Player) -> Maybe PlayerActions
    filterActions (PlayerJump, Player{..})
        | snd pl'Pos > 0 = Nothing
    filterActions (act, _) = Just act

objGravity :: SF (Float, Float) (Float, Float)
objGravity =
    second $ arr (\y -> y - 1) -- >>> integral

playerPict :: SF Player Picture
playerPict = arrPrim drawPlayer
  where
    drawPlayer :: Player -> Picture
    drawPlayer p@(Player (x, y) _) = translate x y $ color black $ form p

    form :: Player -> Picture
    form Player{..}
        | pl'JumpsC > 5 = rectangleSolid 20 20
        | otherwise = circleSolid 10

displayPlayer :: SF (Event G.Event) Picture
displayPlayer = playerEvents >>> playerAct >>> playerPict
