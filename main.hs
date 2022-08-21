import Data.List

-- s/c = substitution with a counter


data Term a = Val a | Var Int deriving (Show, Eq)

type Subst a = [(Int, Term a)]

data SubstC a = SubstC Int (Subst a)

walk :: Term a -> Subst a -> Term a
walk (Val val) _ = Val val
walk (Var i) subst =
     case lookup i subst of 
       Just term -> walk term $ flip filter subst $ (i /=) . fst
       Nothing -> Var i

-- TODO: check for circularity
extSubst :: Int -> Term a -> Subst a -> Subst a
extSubst i term = (:) $ (,) i term

data Tree a = Leaf (Term a) | Node (Term a, Term a) deriving (Show, Eq)

unify :: Eq a => Tree a -> Tree a -> Subst a -> Maybe (Subst a)
unify (Leaf a) (Leaf b) s =
    case (a, b) of
      (Val a, Val b) -> if a == b then Just s else Nothing
      (Var i, term) -> Just $ extSubst i term s
      (a, b) -> unify (Leaf b) (Leaf a) s
unify (Node (l1, r1)) (Node (l2, r2)) s = do
    s2 <- unify (Leaf $ walk l1 s) (Leaf $ walk l2 s) s
    unify (Leaf $ walk l1 s2) (Leaf $ walk l2 s2) s2
unify _ _ _ = Nothing

-- eq :: Eq a => Tree a -> Tree a -> 


main = do
    print $ walk (Var 0) [(2, Val "cat"), (1, Var 2), (0, Var 1)]
    print $ extSubst 2 (Val "cat") [(1, Var 2), (0, Var 1)]
    print $ unify (Leaf (Var 0)) (Leaf (Val 3)) []
    print $ unify (Node (Var 0, Var 3)) (Leaf (Val 3)) []
    print $ unify (Node (Var 3, Var 0)) (Node (Val "lkj", Val "hjk")) []
    print $ unify (Node (Val "lkj", Val "hjk")) (Node (Var 0, Var 3)) []

