import Data.List
import Data.Maybe
import Data.Function
import Control.Monad

data Term a = Var Int | Atom a | Pair (Term a) (Term a) deriving (Eq, Show)

type Subst a = [(Int, Term a)]
type SC a = (Int, (Subst a))
type Goal a = SC a -> [SC a]

unify :: Eq a => Subst a -> Term a -> Term a -> [Subst a]
unify s (Atom a) (Atom b) = if a == b then [s] else []
unify s (Var i) term = [(i, term):s]
unify s term (Var i) = unify s (Var i) term
unify s (Pair l1 r1) (Pair l2 r2) = bar l1 l2 s >>= bar r1 r2 where
    bar a b s = (on (unify s) $ walk s) a b
    walk s t@(Var i) = maybe t (walk xs) $ lookup i s where
        xs = filter ((i/=) . fst) s
    walk _ term = term
unify _ _ _ = []

eq :: Eq a => Term a -> Term a -> Goal a
eq a b (c, s) = map ((,) c) $ unify s a b

callFresh :: (Term a -> Goal a) -> Goal a
callFresh f (c, s) = (f (Var c)) ((c+1), s)

disj :: Goal a -> Goal a -> Goal a
disj g1 g2 = \cs -> g1 cs ++ g2 cs

conj :: Goal a -> Goal a -> Goal a
conj = (>=>)

unify' = unify []

main = do
    print $ unify' (Var 0) (Atom "a")
    print $ unify' (Pair (Var 3) (Var 0)) (Pair (Atom "lkj") (Atom "hjk"))
    print $ unify' (Pair (Atom "lkj") (Atom "hjk")) (Pair (Var 0) (Var 3))
    print $ unify' (Pair (Pair (Atom "3") (Atom "2")) (Atom "0"))
                  (Pair (Pair (Var 0) (Var 2)) (Var 3))
    print $ eq (Pair (Pair (Atom "a") (Var 0)) (Var 1))
               (Pair (Pair (Var 0) (Var 1)) (Var 2)) (0, [])
    print $ callFresh (\x -> eq x (Atom "cat")) (0, [])

    print $ disj 
        (callFresh (\x -> eq x (Atom "cat")))
        (callFresh (\y -> eq y (Atom "dog")))
        (0, [])

    print $ conj 
        (callFresh (\x -> eq x (Atom "cat")))
        (callFresh (\y -> eq y (Atom "dog")))
        (0, [])

