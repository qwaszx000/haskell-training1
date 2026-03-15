module Zipper where

import Control.Monad.State (MonadState, StateT, modify, runState)

-- https://en.wikipedia.org/wiki/Zipper_(data_structure)
-- https://wiki.haskell.org/index.php?title=Zipper_monad
-- https://wiki.haskell.org/Zipper
-- https://blog.ezyang.com/2010/04/you-could-have-invented-zippers/
-- https://en.wikibooks.org/wiki/Haskell/Zippers
-- https://learnyouahaskell.github.io/zippers.html

-- List zipper

newtype ListZipper a = ListZipper ([a], [a])
    deriving (Show)

createLz :: [a] -> ListZipper a
createLz = ListZipper . ([],)

lzLeft :: ListZipper a -> ListZipper a
lzLeft (ListZipper (x : xs, ys)) = ListZipper (xs, x : ys)
lzLeft lz = lz

lzRight :: ListZipper a -> ListZipper a
lzRight (ListZipper (xs, y : ys)) = ListZipper (y : xs, ys)
lzRight lz = lz

lz2list :: ListZipper a -> [a]
lz2list (ListZipper ([], ys)) = ys
lz2list lz = lz2list $ lzLeft lz

lzInsert :: ListZipper a -> a -> ListZipper a
lzInsert (ListZipper (xs, ys)) val = ListZipper (val : xs, ys)

lzRemove :: ListZipper a -> ListZipper a
lzRemove (ListZipper (x : xs, ys)) = ListZipper (xs, ys)
lzRemove lz = lz

instance Functor ListZipper where
    fmap :: (a -> b) -> ListZipper a -> ListZipper b
    fmap f (ListZipper (xs, ys)) = ListZipper (fmap f xs, fmap f ys)

instance Foldable ListZipper where
    foldr :: (a -> b -> b) -> b -> ListZipper a -> b
    foldr f acc lz = foldr f acc $ lz2list lz

{-
class (Monad m) => ListZipperMonad m where
    lzmLeft :: m ()
    lzmRight :: m ()
    lzmInsert :: s -> m ()
    lzmRemove :: m ()

instance (Monad m) => ListZipperMonad (StateT (ListZipper a) m) where
  lzmLeft :: Monad m => StateT (ListZipper a) m ()
  lzmLeft = modify lzLeft
  lzmRight :: Monad m => StateT (ListZipper a) m ()
  lzmRight = modify lzRight
  lzmInsert :: Monad m => s -> StateT (ListZipper s) m ()
  lzmInsert val = modify $ (flip lzInsert) val
  lzmRemove :: Monad m => StateT (ListZipper a) m ()
  lzmRemove = _
-}

lzmLeft :: (MonadState (ListZipper s) m) => m ()
lzmLeft = modify lzLeft

lzmRight :: (MonadState (ListZipper s) m) => m ()
lzmRight = modify lzRight

lzmInsert :: (MonadState (ListZipper s) m) => s -> m ()
lzmInsert val = modify $ flip lzInsert val

lzmRemove :: (MonadState (ListZipper s) m) => m ()
lzmRemove = modify lzRemove

testLZ :: IO ()
testLZ = do
    let initL = [1, 2, 3, 4, 5]
    print initL
    let (_, resL) = runState testAct $ createLz initL
    print $ lz2list resL
  where
    testAct :: (MonadState (ListZipper Int) m) => m ()
    testAct = do
        lzmRight
        lzmInsert 5
        lzmRight
        lzmRemove
        lzmRight
        lzmInsert 5
        lzmLeft
        lzmRemove
        pure ()

-- Unbalanced Binary tree zipper

data BinTree a = BNode a (BinTree a) (BinTree a) | BNil

-- data BinTree a = BNode a (Maybe (BinTree a)) (Maybe (BinTree a))

instance (Show a) => Show (BinTree a) where
    show :: (Show a) => BinTree a -> String
    show BNil = "BNil"
    show (BNode val lt gt) = show val <> "\n" <> lts <> gts
      where
        lts = unlines $ map ("-" <>) $ lines $ show lt
        gts = unlines $ map ("-" <>) $ lines $ show gt

binInsert :: (Ord a) => a -> BinTree a -> BinTree a
binInsert val BNil = BNode val BNil BNil
binInsert val tree@(BNode cval ltree gtree)
    | val == cval = tree
    | val < cval = BNode cval (binInsert val ltree) gtree
    | val > cval = BNode cval ltree (binInsert val gtree)

binDelete :: (Ord a) => a -> BinTree a -> BinTree a
binDelete _ BNil = BNil
binDelete val tree@(BNode cval BNil BNil)
    | val == cval = BNil
    | otherwise = tree
binDelete val (BNode cval lt gt)
    | val == cval = lt `binCombine` gt
    | val > cval = BNode cval lt (binDelete val gt)
    | val < cval = BNode cval (binDelete val lt) gt

-- Don't blame me...
binCombine :: (Ord a) => BinTree a -> BinTree a -> BinTree a
binCombine t1 BNil = t1
binCombine BNil t1 = t1
binCombine (BNode lval llt lgt) (BNode gval glt ggt) =
    BNode
        lval
        (llt `binCombine` binInsert gval glt)
        (lgt `binCombine` ggt)

data BTZContext a = BTZLeft a (BinTree a) | BTZRight a (BinTree a)
type BTZipper a = (BinTree a, [BTZContext a])

btz2bt :: BTZipper a -> BinTree a
btz2bt (tree, []) = tree
btz2bt z@(tree, c : cs) = btz2bt $ btzUp z

createBTZ :: BinTree a -> BTZipper a
createBTZ = (,[])

btzUp :: BTZipper a -> BTZipper a
btzUp z@(tree, []) = z
btzUp (tree, c : cs) = (,cs) $ case c of
    BTZLeft val rtree -> BNode val tree rtree
    BTZRight val ltree -> BNode val ltree tree

btzLeft :: BTZipper a -> BTZipper a
btzLeft (BNode val ltree rtree, cs) = (ltree, BTZLeft val rtree : cs)
btzLeft z = z

btzRight :: BTZipper a -> BTZipper a
btzRight (BNode val ltree rtree, cs) = (rtree, BTZRight val ltree : cs)
btzRight z = z

btzInsert :: (Ord a) => a -> BTZipper a -> BTZipper a
btzInsert val (tree, cs) = (binInsert val tree, cs)

btzReplace :: a -> BTZipper a -> BTZipper a
btzReplace val (tree, cs) = (,cs) $ case tree of
    BNode _ lt gt -> BNode val lt gt
    BNil -> BNode val BNil BNil

btzDelete :: BTZipper a -> BTZipper a
btzDelete (_, cs) = (BNil, cs)

btzmUp :: (MonadState (BTZipper s) m) => m ()
btzmUp = modify btzUp

btzmLeft :: (MonadState (BTZipper s) m) => m ()
btzmLeft = modify btzLeft

btzmRight :: (MonadState (BTZipper s) m) => m ()
btzmRight = modify btzRight

-- btzmInsert :: (Ord s) => (MonadState (BTZipper s) m) => s -> m ()
btzmInsert :: (Ord s, MonadState (BTZipper s) m) => s -> m ()
btzmInsert val = modify $ btzInsert val

btzmRemove :: (MonadState (BTZipper s) m) => m ()
btzmRemove = modify btzDelete

btzmReplace :: (MonadState (BTZipper s) m) => s -> m ()
btzmReplace val = modify $ btzReplace val

testBTZ :: IO ()
testBTZ = do
    print initBT
    let (_, resBT) = runState testAct $ createBTZ initBT
    print $ btz2bt resBT
  where
    initBT :: BinTree Int
    initBT =
        BNode
            5
            ( BNode
                6
                (BNode 7 BNil BNil)
                BNil
            )
            (BNode 10 BNil BNil)

    testAct :: (MonadState (BTZipper Int) m) => m ()
    testAct = do
        btzmReplace 1
        btzmLeft
        btzmLeft
        btzmInsert 8
        btzmReplace 88
        btzmUp
        btzmUp
        btzmRight
        btzmInsert 5
        btzmLeft
        btzmReplace 99
        btzmUp
        btzmReplace 100
