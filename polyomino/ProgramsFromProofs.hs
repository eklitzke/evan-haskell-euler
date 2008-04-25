-- ProgramsFromProofs.hs

import Gentzen
import List (delete, (\\), sort, insert)
import Maybe (fromJust)

data Term = Var Int |
            Lambda Term Term |                   -- \x -> t - where x is usually a variable but sometimes a pair
            Apply Term Term |
            Pair Term Term |                     -- (t,u)
            Fst Term |
            Snd Term |
            Case Term (Term,Term) (Term,Term) |  -- case x of Left l -> t; Right r -> u
            Inl Term |                           -- Left t
            Inr Term |                           -- Right t
            Undef
            deriving (Eq,Ord)

instance Show Term where
    show s = case s of
             Var i              -> "x" ++ show i
             Lambda x t         -> case t of
                                   Lambda _ _ -> "\\" ++ show x ++ " " ++ tail (show t)
                                   otherwise  -> "\\" ++ show x ++ " -> " ++ show t -- we could try to show "_" if x isn't used in t - but watch out if x is a pair
             Apply t u          -> brackets t ++ " " ++ brackets u
             Pair t u           -> "(" ++ show t ++ "," ++ show u ++ ")"
             Fst t              -> "fst " ++ brackets t
             Snd t              -> "snd " ++ brackets t
             Case t (x,u) (y,v) -> "case " ++ show t ++ " of Left " ++ show x ++ " -> " ++ show u ++ "; Right " ++ show y ++ " -> " ++ show v
             Inl t              -> "Left " ++ brackets t
             Inr t              -> "Right " ++ brackets t
             Undef              -> "undefined"

brackets t = case t of
             Var _     -> show t
             Pair _ _  -> show t
             Undef     -> show t
             otherwise -> "(" ++ show t ++ ")"

subst t (u,u')
    | t == u    = u'
    | otherwise = case t of
        Var n      -> Var n
        Lambda x v -> Lambda x (v `subst` (u,u'))
        Apply v w  -> Apply (v `subst` (u,u')) (w `subst` (u,u'))
        Pair v w   -> Pair (v `subst` (u,u')) (w `subst` (u,u'))
        Fst v      -> Fst (v `subst` (u,u'))
        Snd v      -> Snd (v `subst` (u,u'))
        Case s (x,v) (y,w) -> Case (s `subst` (u,u')) (x `subst` (u,u'), v `subst` (u,u')) (y `subst` (u,u'), w `subst` (u,u'))
        Inl v      -> Inl (v `subst` (u,u'))
        Inr v      -> Inr (v `subst` (u,u'))
        Undef      -> Undef

-- LeftConj and LeftImpl rules to help Curry-Howard go through

-- We have to use the G3ip LeftConj rule, rather than the following, otherwise we can't prove (a->b)&(b->c) -> (a->c)
-- leftConjCH (Seq as [r]) = zip (concat [ let as' = delete f as in [ [Seq (p:as') [r]], [Seq (q:as') [r]] ] | f@(Conj p q) <- as]) (repeat LeftConj)

leftImplCH (Seq as [r]) = zip [ let as' = delete f as in [Seq as' [p], Seq (q:as') [r]] | f@(Impl p q) <- as] (repeat LeftImpl)

-- This leads to a system weaker than G3/4ip - for example, it seems the full version of leftImpl is required to prove ¬¬pierce'sLaw

tacticsCH = axiomG3ip +++ leftConjG3ip +++ rightConjG3ip +++ leftDisjG3ip +++ rightDisjG3ip +++ leftImplCH +++ rightImplG3ip
            -- +++ leftFalseG3ip
proofCH = proof tacticsCH

-- principal (PT (Seq _ [p]) Axiom []) = p
-- principal (PT (Seq _ [f@(Impl p q)]) RightImpl _) = f
principal (PT (Seq as [p]) LeftImpl [l@(PT (Seq bs _) _ _),r]) = head (as \\ bs)
-- principal (PT (Seq _ [f@(Conj p q)]) RightConj _) = f
-- principal (PT (Seq as [p]) LeftConj [PT (Seq bs _) _ _]) = head (as \\ bs)
-- principal (PT (Seq _ [f@(Disj p q)]) RightDisj _) = f
principal (PT (Seq as [p]) LeftDisj [l@(PT (Seq bs _) _ _),r]) = head (as \\ bs)


-- only intended to work for G3CH proofs
{-
-- !! not working yet
-- finding terms for proofs involving false/negation
termFromProof (PT (Seq as [p]) LeftFalse [], i) =
    let as' = zip (sort as) (map Var [i..])
        j = i + length as
        v = case lookup p as' of
            Just v' -> v'
            Nothing -> Var j
        p' = (p,v)
    in (Seq as' [p'], j+1)
-- so we assign a var of type F
-}
termFromProof (PT (Seq as [p]) Axiom [], i) =
    let as' = zip (sort as) (map Var [i..])
        Just v = lookup p as'
        p' = (p,v)
    in (Seq as' [p'], i + length as)
-- If there are duplicate occurrences of some type among the assumptions, we assign different variables. Try a->a->a to see why.
-- we sort the assumptions, and use order-preserving insert and delete hereafter, to make it easier to cross-substitute when combining branches

termFromProof (PT (Seq as [Impl p q]) RightImpl [child], i) =
    let (Seq pas [q'@(_,m)],j) = termFromProof (child,i)
        Just x = lookup p pas
        as' = delete (p,x) pas
    in (Seq as' [(Impl p q, Lambda x m)], j)

termFromProof (proof@(PT (Seq as [r]) LeftImpl [first, second]), i) =
    let f@(Impl p q) = principal proof
        (Seq as' [p'@(_,m)],j) = termFromProof (first,i)
        (Seq bs [r'@(_,n)],k) = termFromProof (second,j)
        Just x = lookup q bs
        bs' = delete (q,x) bs
        y = Var k
        n' = foldl subst n (zip (map snd bs') (map snd as'))
    in (Seq (insert (f,y) as') [(r, n' `subst` (x,Apply y m))], k+1) -- we use insert to make sure that the assumptions are in order - this makes substitutions easier

termFromProof (PT (Seq as [Conj p q]) RightConj [first,second], i) =
    let (Seq as' [p'@(_,m)],j) = termFromProof (first,i)
        (Seq bs' [q'@(_,n)],k) = termFromProof (second,j)
        n' = foldl subst n (zip (map snd bs') (map snd as'))
    in (Seq as' [(Conj p q, Pair m n')], k)

termFromProof (proof@(PT (Seq as [r]) LeftConj [child@(PT (Seq bs _) _ _)]), i) =
    let Conj p q = head (as \\ bs) -- the principal formula
        (Seq as' [(_,m)],j) = termFromProof (child,i)
        Just x = lookup p as'
        as'' = delete (p,x) as' -- defect fix - when p==q we need to get *different* variables, so we must delete first result before doing second lookup
        Just y = lookup q as''
        as''' = insert (Conj p q, Pair x y) $ delete (q,y) as''
    in ( Seq as''' [(r, m)], j)
{-
termFromProof (proof@(PT (Seq as [r]) LeftConj [child@(PT (Seq bs _) _ _)]), i) =
    let Conj p q = principal proof
        (Seq as' [(_,m)],j) = termFromProof (child,i)
        Just x = lookup p as'
        Just y = lookup q as'
        z = Var j
    in ( Seq ( insert (Conj p q, z) $               -- new variable of type p&q
               -- as' \\ [(p,x),(q,y)] ) -- this might delete multiple copies, whereas delete only deletes one
               delete (p,x) $ delete (q,y) as' )
             [(r, m `subst` (x, Fst z) `subst` (y, Snd z))],
        j+1)
-}
{-
-- old version using a weaker version of LeftConj rule
termFromProof (proof@(PT (Seq as [r]) LeftConj [child@(PT (Seq bs _) _ _)]), i) =
    let active = head (bs \\ as)
        Conj p q = principal proof
        (Seq as' [(_,m)],j) = termFromProof (child,i)
        Just x = lookup active as'
        y = Var j
    in ( Seq ( insert (Conj p q, y) $               -- new variable of type p&q
               delete (active,x) as' )
             [(r, m `subst` (x, if active == p then Fst y else Snd y))], -- new termFromProof of type r
        j+1)
-}
termFromProof (PT (Seq as [Disj p q]) RightDisj [child@(PT (Seq _ [r]) _ _)], i) =
    let (Seq as' [r'@(_,m)],j) = termFromProof (child,i)
    in (Seq as' [(Disj p q, if r == p then Inl m else Inr m)], j)

termFromProof (proof@(PT (Seq as [r]) LeftDisj [first,second]), i) =
    let Disj p q = principal proof
        (Seq bs [(_,m)],j) = termFromProof (first,i)
        (Seq cs [(_,n)],k) = termFromProof (second,j)
        Just x = lookup p bs
        Just y = lookup q cs
        bs' = delete (p,x) bs
        cs' = delete (q,y) cs
        n' = foldl subst n (zip (map snd cs') (map snd bs'))
        z = Var k
    in ( Seq ( insert (Disj p q, z) bs' )               -- new variable of type p|q
             [(r, Case z (x,m) (y,n'))], -- new term of type r
        k+1)

witness p = case proofCH ([] |- [p]) of
            Just proof -> let (Seq _ [(p',t)], _) = termFromProof (proof,1) in Just t
            Nothing    -> Nothing



