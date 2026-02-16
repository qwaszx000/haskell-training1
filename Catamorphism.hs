module Catamorphism where

import Data.Foldable (foldl')
import Data.List (partition)
import Data.Map (Map, fromList, (!))

-- https://quentinduval.github.io/blog/2017/01/20/catamorph-dsl-deep-dive.html

data Operator = Add | Mul
    deriving (Show, Eq)

newtype Fix t = Fix {unFix :: t (Fix t)}

type Expr = Fix ExprR

type Id = Char
type Env = Map Id Int
data ExprR r
    = Const Int
    | Var Id
    | Op Operator [r]
    deriving (Eq)

instance Functor ExprR where
    fmap _ (Const n) = Const n
    fmap _ (Var i) = Var i
    fmap f (Op op rs) = Op op $ map f rs

econst :: Int -> Expr
econst = Fix . Const

evar :: Id -> Expr
evar = Fix . Var

eop :: Operator -> [Expr] -> Expr
eop op exs = Fix $ Op op exs

testExp =
    eop
        Add
        [ eop Mul [econst 5, econst 7, evar 'c', econst 6]
        , eop Add [econst 54, econst 7, evar 'c', econst 8]
        , econst 5
        ]

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

type Algebra f = f (Fix f) -> Fix f

comp :: Algebra f -> Algebra f -> Algebra f
comp f g = f . unFix . g

printExpr :: Expr -> String
printExpr = cata algebra
  where
    algebra :: ExprR String -> String
    algebra (Const n) = show n
    algebra (Var c) = ['"', c]
    algebra (Op op strs) = "(" ++ show op ++ " " ++ unwords strs ++ ")"

optimizeAdd :: ExprR Expr -> Expr
optimizeAdd ex@(Op Add exs) = optimizeOp ex 0 (+)
optimizeAdd ex = Fix ex

isZeroCst :: Expr -> Bool
isZeroCst (Fix (Const 0)) = True
isZeroCst _ = False

optimizeMul :: ExprR Expr -> Expr
optimizeMul ex@(Op Mul exs)
    | any isZeroCst exs = econst 0
    | otherwise = optimizeOp ex 1 (*)
optimizeMul ex = Fix ex

optimizeOp :: ExprR Expr -> Int -> (Int -> Int -> Int) -> Expr
optimizeOp (Op op exs) neutral combine =
    let
        isCst (Const _) = True
        isCst _ = False
        (consts, vars) = partition (isCst . unFix) exs
        consts' = map (\(Fix (Const n)) -> n) consts
        constsTotal = foldl' combine neutral consts'
     in
        case vars of
            [] -> econst constsTotal
            [x] | constsTotal == neutral -> x
            xs | constsTotal == neutral -> Fix $ Op op xs
            xs -> Fix $ Op op (econst constsTotal : xs)

optimize :: Expr -> Expr
optimize = cata (optimizeAdd `comp` optimizeMul)

main :: IO ()
main = do
    let env = fromList [('c', 5)]

    -- putStrLn $ printExpr testExp
    putStrLn $ printExpr $ optimize testExp

-- print testExp
-- let ex = optimize testExp
-- print ex

-- let ex' = substitute env ex
-- print ex'

-- print $ optimize ex'
-- print $ eval env testExp
