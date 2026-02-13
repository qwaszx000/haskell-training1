-- {-# LANGUAGE DatatypeContexts #-}

module TypeClasses where

-- Or data
-- Requires {-# LANGUAGE DatatypeContexts #-}
-- newtype (Num a) => MyNum a = MyNum a
--     deriving (Show)

class Alive a where
    -- Method is not defined, so it is required to be defined in instance block
    -- But you'll only get warning if it is not defined in instance
    -- Yet it will result in runtime error if this method is called in this case
    getHealth :: a -> Int

class Warrior w where
    getDamage :: w -> Int

class (Alive s, Warrior s) => Soldier s where
    fight :: s -> (Int, Int)
    fight s = (getHealth s, getDamage s)

    fight2 :: s -> (Int, Int)
    fight2 s = (getHealth s, getDamage s)

data Monk a = Monk
    { hp :: Int
    , damage :: Int
    , monkRang :: a
    }

instance Alive (Monk a) where
    getHealth = hp

instance Warrior (Monk a) where
    getDamage = damage

-- This is required for Ord instance
-- Otherwise you get error: Could not deduce ‘Eq (Monk a)’
-- And: No instance for ‘Eq (Monk a0)’ arising from a use of ‘==’
instance (Eq a) => Eq (Monk a) where
    m1 == m2 = monkRang m1 == monkRang m2

instance (Ord a) => Ord (Monk a) where
    m1 `compare` m2 = monkRang m1 `compare` monkRang m2

-- Without overwriting any methods
-- instance Soldier (Monk a)

-- With overwriting defined method
instance Soldier (Monk a) where
    fight2 s = (getHealth s + 5, getDamage s + 6)

monkTest :: IO ()
monkTest = do
    let m = Monk{hp = 5, damage = 2, monkRang = (+)}
    let m1 = Monk{hp = 5, damage = 2, monkRang = 1}
    let m2 = Monk{hp = 2, damage = 5, monkRang = 1}

    -- m == m1 -- error, can't compare because (+) does not implement Eq

    print $ fight m1
    print $ fight2 m1
    print $ m1 == m2
