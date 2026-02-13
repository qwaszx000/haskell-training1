module SmartConstructors where

-- https://wiki.haskell.org/index.php?title=Smart_constructors
-- https://wiki.haskell.org/index.php?title=Type_arithmetic
-- https://wiki.haskell.org/index.php?title=Peano_numbers
-- Maybe https://wiki.haskell.org/index.php?title=Functional_dependencies

-- Also
-- https://wiki.haskell.org/index.php?title=Existential_type
-- https://wiki.haskell.org/index.php?title=Type_witness
-- https://nikita-volkov.github.io/refined/
-- https://ucsd-progsys.github.io/liquidhaskell/
-- Are interesting

data Z = Z
data S a = S a

class MyNum n

instance MyNum Z
instance (MyNum n) => MyNum (S n)

type S1 = S Z
type S2 = S S1
type S3 = S S2
type S4 = S S3

class (MyNum s) => AcceptableSize s where
    createResistor :: s -> Resistor s

instance AcceptableSize S1 where
    createResistor _ = Resistor 1

instance AcceptableSize S2 where
    createResistor _ = Resistor 2

d0 = undefined :: Z
d1 = undefined :: S1
d2 = undefined :: S2
d3 = undefined :: S3
d4 = undefined :: S4

-- Acceptable size > 0 && < 3
-- sizeT is a phantom size type for type checking
-- and size field is for runtime usage
data Resistor sizeT = Resistor {size :: Int}
    deriving (Show)

-- createResistor d0
-- No instance for ‘AcceptableSize Z’
-- createResistor d1
-- Resistor {size = 1}
-- createResistor d2
-- Resistor {size = 2}
-- createResistor d3
-- No instance for ‘AcceptableSize S3’
