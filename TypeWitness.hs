{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module TypeWitness where

-- https://serokell.io/blog/haskell-type-level-witness
-- https://hacklewayne.com/type-witness-not-yet-typescript-sure-thing-haskell
-- https://wiki.haskell.org/Type_witness

data UserPrivilege = Admin | User
    deriving (Show)

data WitnessPriv priv where
    WitnessAdmin :: WitnessPriv Admin
    WitnessUser :: WitnessPriv User

data UserRec (priv :: UserPrivilege) = UserRec
    { name :: String
    , counter :: Int
    , pr :: WitnessPriv priv
    }

data SomeUser where
    SomeUser :: UserRec a -> SomeUser

showName :: UserRec any -> String
showName UserRec{name = nameStr} = nameStr

incCounter :: UserRec 'Admin -> UserRec 'Admin
incCounter u@UserRec{counter = c} = u{counter = c + 1}

-- This function must be IO, otherwise haskell tries to infer type on it and it breaks type witness
createTestUser :: IO SomeUser
createTestUser = pure $ SomeUser $ UserRec "Test" 0 WitnessAdmin

main :: IO ()
main = do
    SomeUser user <- createTestUser

    putStrLn $ showName user

    _ <- case pr user of
        WitnessAdmin -> do
            let user' = incCounter user
            pure 0
        _ -> error "Wrong privilege"
    pure ()
