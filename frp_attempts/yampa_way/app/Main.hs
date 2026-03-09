{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Data.IORef

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import FRP.Yampa

-- https://github.com/gelisam/frp-zoo/blob/master/Yampa-example/Main.hs

main :: IO ()
main =
    playYampa
        dmode
        bgColor
        20
        mainSF

mainSF :: SF (Event G.Event) Picture
mainSF = _

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
        1.0
        (const $ readIORef pic)
        ( \ev t -> do
            _ <- react rhand (eventInterval, Just $ Event ev)
            pure $ t + eventInterval
        )
        (\dt t -> pure $ realToFrac dt + t)
  where
    eventInterval :: Double
    eventInterval = 0.01 / fromIntegral freq

dmode :: Display
dmode = InWindow "My title" (200, 200) (20, 20)

bgColor :: Color
bgColor = white

displayWorld :: World -> Picture
displayWorld (World pl cur) = pictures [displayPlayer pl, displayCursor cur]

displayCursor :: Cursor -> Picture
displayCursor (Cursor{..}) = translate x y $ color (makeColor 0.5 0.5 0 0.6) $ circleSolid cursorSize
  where
    (x, y) = cursorPos

displayPlayer :: Player -> Picture
displayPlayer Player{..} = translate x y $ color black form
  where
    (x, y) = playerPos
    form
        | playerJumpsC > 5 = rectangleSolid 20 20
        | otherwise = circleSolid 10

data World = World Player Cursor
data Player = Player {playerPos :: (Float, Float), playerCanJump :: Bool, playerJumpsC :: Int}
data Cursor = Cursor {cursorPos :: (Float, Float), cursorSize :: Float}

initWorld :: World
initWorld = World (Player (0, 0) True 0) (Cursor (0, 0) 10)

stepWorld :: Float -> World -> World
stepWorld _ w@(World pl cur) =
    if not $ playerCanJump pl
        then
            let
                (x, y) = playerPos pl
                newPos = (x, y - 1)
                canJump = y == 0
                pl' = pl{playerPos = newPos, playerCanJump = canJump}
             in
                World pl' cur
        else w

handleEvents :: G.Event -> World -> World
handleEvents (G.EventMotion (x, y)) (World pl cur) = w'
  where
    w' = World pl cur'
    cur' = cur{cursorPos = (x, y)}
handleEvents (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) w@(World pl cur) =
    if playerCanJump pl
        then
            let
                (x, y) = playerPos pl
                c' = (playerJumpsC pl + 1) `mod` 10
                pl' = pl{playerCanJump = False, playerPos = (x, y + 10), playerJumpsC = c'}
             in
                World pl' cur
        else w
handleEvents _ w = w
