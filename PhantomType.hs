module PhantomType where

data Unvalidated
data Validated

-- a is a phantom type
newtype Password a = Password String

getPassword :: String -> Password Unvalidated
getPassword = Password

validatePassword :: Password Unvalidated -> Password Validated
-- validatePassword p = p -- Couldn't match type ‘Unvalidated’ with ‘Validated’
validatePassword (Password str) = Password str

usePassword :: Password Validated -> String
usePassword (Password str) = str

testPhantom :: IO ()
testPhantom = do
    let pu = getPassword "hello"
    let pv = validatePassword pu

    -- let pv' = validatePassword pv -- Couldn't match type ‘Validated’ with ‘Unvalidated’
    -- print $ usePassword pu -- Couldn't match type ‘Unvalidated’ with ‘Validated’
    print $ usePassword pv
