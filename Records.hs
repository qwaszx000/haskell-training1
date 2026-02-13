module Records where

import Data.Tuple (swap)

tuple_test :: IO ()
tuple_test = do
    let tuple1 = (1, "Hello")
    print tuple1
    let tuple2 = swap tuple1
    print tuple1
    print tuple2

    let tuple3 = myAddEl [1, 2] tuple2
    print tuple3

myAddEl :: c -> (a, b) -> (a, b, c)
myAddEl c (a, b) = (a, b, c)

---

data Person = Person
    { name :: String
    , age :: Int
    , hash :: String
    }
    deriving (Show)

calcHash :: Person -> String
calcHash p = (name p) ++ (show $ age p)

recordsTest :: IO ()
recordsTest = do
    -- all fields must be inited
    let p1 = Person{name = "Name", age = 25, hash = ""}
    print p1
    -- Set field
    -- Can't do p1 = p1{...} because it causes infinite recursion
    let p1' = p1{hash = calcHash p1}
    -- Can shadow p1 here
    let p1 = p1'
    let p1' = p1{age = age p1 + 1}
    print p1
    print p1'

    putStrLn $ prettyPrint p1

prettyPrint :: Person -> String
prettyPrint (Person{name = n, age = a}) =
    "Name: "
        ++ n
        ++ "\n"
        ++ "Age: "
        ++ show a
        ++ "\n"
