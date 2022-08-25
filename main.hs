import Data.List
import Data.Maybe
import Data.Function
import Control.Monad

data Term a = Var Int | Atom a | P (Term a) (Term a) deriving (Eq, Show)

type Subst a = [(Int, Term a)]
type SC a = (Int, (Subst a))
type Goal a = SC a -> [SC a]

walk :: Subst a -> Term a -> Term a
walk s t@(Var i) = maybe t (walk xs) $ lookup i s where
    xs = filter ((i/=) . fst) s
walk _ term = term

unify :: Eq a => Term a -> Term a -> Subst a -> [Subst a]
unify t1 t2 s = case walk s <$> (t1, t2) of 
    (Atom a, Atom b) -> if a == b then [s] else []
    (Var i , term  ) -> [(i, term):s]
    (term  , Var i ) -> [(i, term):s]
    (P a c , P b d ) -> unify a b s >>= unify c d
    _                -> []

eq :: Eq a => Term a -> Term a -> Goal a
eq a b (c, s) = (,) c <$> unify a b s

callFresh :: (Term a -> Goal a) -> Goal a
callFresh f (c, s) = (f $ Var c) (c+1, s)

disj :: Goal a -> Goal a -> Goal a
disj g1 g2 = \cs -> g1 cs ++ g2 cs

conj :: Goal a -> Goal a -> Goal a
conj = (>=>)

unify' a b = unify a b []

main = do
    print $ unify' (Var 0) (Atom "a")
    print $ unify' (P (Var 3) (Var 0)) (P (Atom "lkj") (Atom "hjk"))
    print $ unify' (P (Atom "lkj") (Atom "hjk")) (P (Var 0) (Var 3))
    print $ unify' (P (P (Atom "3") (Atom "2")) (Atom "0"))
                  (P (P (Var 0) (Var 2)) (Var 3))
    print $ eq (P (P (Atom "a") (Var 0)) (Var 1))
               (P (P (Var 0) (Var 1)) (Var 2)) (0, [])
    print $ callFresh (\x -> eq x (Atom "cat")) (0, [])

    print $ disj 
        (callFresh (\x -> eq x (Atom "cat")))
        (callFresh (\y -> eq y (Atom "dog")))
        (0, [])

    print $ conj 
        (callFresh (\x -> eq x (Atom "cat")))
        (callFresh (\y -> eq y (Atom "dog")))
        (0, [])

