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

-- displayWorld :: World -> Picture
-- displayWorld (World pl cur) = pictures [displayPlayer pl, displayCursor cur]

-- displayPlayer :: Player -> Picture
-- displayPlayer Player{..} = translate x y $ color black form
--   where
--     (x, y) = playerPos
--     form
--         | playerJumpsC > 5 = rectangleSolid 20 20
--         | otherwise = circleSolid 10

-- stepWorld :: Float -> World -> World
-- stepWorld _ w@(World pl cur) =
--     if not $ playerCanJump pl
--         then
--             let
--                 (x, y) = playerPos pl
--                 newPos = (x, y - 1)
--                 canJump = y == 0
--                 pl' = pl{playerPos = newPos, playerCanJump = canJump}
--              in
--                 World pl' cur
--         else w

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

-- data Player = Player {pl'Pos :: (Float, Float), pl'JumpsC :: Int}

jumpEvents :: SF (Event G.Event) (Event PlayerActions)
jumpEvents = arrEPrim $ mapFilterE selectJumps
  where
    selectJumps :: G.Event -> Maybe PlayerActions
    selectJumps (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) = Just PlayerJump
    selectJumps _ = Nothing

initialPlayerPos :: (Float, Float)
initialPlayerPos = (0, 0)

playerPos :: SF (Event PlayerActions) (Float, Float)
playerPos =
    loopPre
        initialPlayerPos
        ( arrPrim
            processPosEvents
            >>> playerGravity
            >>> (identity &&& identity)
        )
  where
    processPosEvents :: (Event PlayerActions, (Float, Float)) -> (Float, Float)
    processPosEvents (Event PlayerJump, (x, y)) = if y == 0 then (x, y + 10) else (x, y)
    processPosEvents (_, (x, y)) = (x, y)

playerGravity :: SF (Float, Float) (Float, Float)
playerGravity =
    second $
        ((identity &&& fallSpeed) >>^ uncurry (-))
            >>> (identity &&& constant 0)
            >>^ uncurry max
  where
    fallSpeed :: SF Float Float
    -- fallSpeed = constant (10 / 1) >>> integral
    fallSpeed = constant 1

playerPict :: SF (Float, Float) Picture
playerPict = arrPrim drawPlayer
  where
    drawPlayer :: (Float, Float) -> Picture
    drawPlayer (x, y) = translate x y $ color black form

    form :: Picture
    form = rectangleSolid 20 20

displayPlayer :: SF (Event G.Event) Picture
displayPlayer = jumpEvents >>> playerPos >>> playerPict

-- playerPict :: SF Player Picture
-- playerPict = arrPrim drawPlayer
--   where
--     drawPlayer :: Player -> Picture
--     drawPlayer p@Player{pl'Pos = (x, y)} = translate x y $ color black (form p)

--     form :: Player -> Picture
--     form Player{..}
--         | pl'JumpsC > 5 = rectangleSolid 20 20
--         | otherwise = circleSolid 10
