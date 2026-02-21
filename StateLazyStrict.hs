module StateLazyStrict where

import Control.Monad (forever)
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS

-- Inspirations
-- https://stackoverflow.com/questions/24072934/haskell-how-lazy-is-the-lazy-control-monad-st-lazy-monad

testLazy :: IO ()
testLazy = do
    let (state, res) = SL.runState lazyStateAction 0
    print state
    print res

lazyStateAction :: SL.State Int String
lazyStateAction = do
    forever $ SL.put 5
    SL.put 6 -- We ignore forever because of laziness
    -- In SO comments they talked about "reading backwards"
    --
    var <- SL.get
    pure $ show var

testStrict :: IO ()
testStrict = do
    let (state, res) = SS.runState strictStateAction 0
    print state
    print res

strictStateAction :: SS.State Int String
strictStateAction = do
    forever $ SS.put 5 -- We hang here
    SS.put 6 -- Doesn't work the same way
    var <- SS.get
    pure $ show var
