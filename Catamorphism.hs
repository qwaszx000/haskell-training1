module Catamorphism where

import Data.Foldable (foldl')
import Data.Map (Map, fromList, (!))

-- https://quentinduval.github.io/blog/2017/01/20/catamorph-dsl-deep-dive.html

data Operator = Add | Mul
    deriving (Show, Eq)

type Id = Char
type Env = Map Id Int
data Expr
    = Const Int
    | Var Id
    | Op Operator [Expr]

-- Basic realisation without Catamorphism, through recursion
-- for Expr without r
instance Show Expr where
    show (Const n) = show n
    show (Var c) = ['"', c]
    show (Op op exs) = "(" ++ show op ++ " " ++ unwords (map show exs) ++ ")"

econst :: Int -> Expr
econst = Const

evar :: Id -> Expr
evar = Var

eop :: Operator -> [Expr] -> Expr
eop = Op

testExp =
    eop
        Add
        [ eop Mul [econst 5, econst 7, evar 'c', econst 6]
        , eop Add [econst 54, econst 7, evar 'c', econst 8]
        , econst 5
        ]

optimize :: Expr -> Expr
optimize (Op op exs) = combine $ foldl' calc (neutral, []) exs
  where
    (neutral, opAction) = case op of
        Add -> (0, (+))
        Mul -> (1, (*))

    -- Const part, list of vars
    combine :: (Int, [Expr]) -> Expr
    combine (cst, []) = econst cst
    combine (cst, exs) = Op op $ econst cst : exs
    ---
    -- Calculate if we can
    calc :: (Int, [Expr]) -> Expr -> (Int, [Expr])
    calc (cst, exs) (Const n) = (cst `opAction` n, exs)
    calc (cst, exs) ex =
        let
            ex' = optimize ex
         in
            case ex' of
                Const n -> (cst `opAction` n, exs)
                _ -> (cst, ex' : exs)
optimize ex = ex

substitute :: Env -> Expr -> Expr
substitute env (Var n) = econst $ env ! n
substitute env (Op op exs) = Op op $ map (substitute env) exs
substitute _ ex = ex

eval :: Env -> Expr -> Int
eval env ex = case optimize $ substitute env ex of
    (Const n) -> n
    _ -> error "Could not eval"

-- cataPrint :: Expr

main :: IO ()
main = do
    let env = fromList [('c', 5)]

    print testExp
    let ex = optimize testExp
    print ex

    let ex' = substitute env ex
    print ex'

    print $ optimize ex'
    print $ eval env testExp
