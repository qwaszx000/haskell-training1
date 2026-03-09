{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main =
    play
        dmode
        bgColor
        20
        initWorld
        displayWorld
        handleEvents
        stepWorld

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

handleEvents :: Event -> World -> World
handleEvents (EventMotion (x, y)) (World pl cur) = w'
  where
    w' = World pl cur'
    cur' = cur{cursorPos = (x, y)}
handleEvents (EventKey (SpecialKey KeyUp) Down _ _) w@(World pl cur) =
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
