import Data.List
import Data.Maybe

data Term a = Var Int | Atom a | Pair (Term a) (Term a) deriving (Eq, Show)

type Subst a = [(Int, Term a)]
type SC a = (Int, (Subst a))
type Goal a = SC a -> [SC a]

walk :: Term a -> Subst a -> Term a
walk (Atom val) _ = Atom val
walk (Var i) subst =
     case lookup i subst of 
       Just term -> walk term $ flip filter subst $ (i /=) . fst
       Nothing -> Var i
walk term _ = term

-- TODO: check for circularity
extSubst :: Int -> Term a -> Subst a -> Subst a
extSubst i term = (:) $ (,) i term

unify :: Eq a => Term a -> Term a -> Subst a -> [Subst a]
unify (Atom a) (Atom b) s = if a == b then [s] else []
unify (Var i) term      s = [extSubst i term s]
unify term (Var i)      s = unify (Var i) term s
unify (Pair l1 r1) (Pair l2 r2) s1 = do
    s2 <- unify (walk l1 s1) (walk l2 s1) s1
    unify (walk r1 s2) (walk r2 s2) s2
unify _ _ _ = []

eq :: Eq a => Term a -> Term a -> Goal a
eq a b (c, s) = map ((,) c)  $ unify (walk a s) (walk b s) s

callFresh :: (Term a -> Goal a) -> Goal a
callFresh f (c, s) = (f (Var c)) ((c+1), s)

disj :: Goal a -> Goal a -> Goal a
disj g1 g2 = \(c, s) -> g1 (c, s) ++ g2 (c, s)

conj :: Goal a -> Goal a -> Goal a
conj g1 g2 = \cs -> g1 cs >>= g2

main = do
    print $ walk (Var 0) [(2, Atom "cat"), (1, Var 2), (0, Var 1)]
    print $ extSubst 2 (Atom "cat") [(1, Var 2), (0, Var 1)]
    print $ unify (Var 0) (Atom "a") []
    print $ unify (Pair (Var 0) (Var 3)) (Atom 3) []
    print $ unify (Pair (Var 3) (Var 0)) (Pair (Atom "lkj") (Atom "hjk")) []
    print $ unify (Pair (Atom "lkj") (Atom "hjk")) (Pair (Var 0) (Var 3)) []
    print $ unify (Pair (Pair (Atom "3") (Atom "2")) (Atom "0"))
                  (Pair (Pair (Var 0) (Var 2)) (Var 3)) []
    print $ eq (Pair (Pair (Atom "a") (Var 0)) (Var 1))
               (Pair (Pair (Var 0) (Var 1)) (Var 2)) (0, [])
    print $ callFresh (\x -> eq x (Atom "cat")) (0, [])
    print $ disj 
        (callFresh (\x -> eq x (Atom "cat")))
        (callFresh (\x -> eq x (Atom "dog")))
        (0, [])
    print $ conj 
        (callFresh (\x -> eq x (Atom "cat")))
        (callFresh (\x -> eq x (Atom "dog")))
        (0, [])

