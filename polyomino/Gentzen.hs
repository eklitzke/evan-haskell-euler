-- Gentzen.hs

-- Source
-- Troelstra, Schwichtenberg, Basic Proof Theory

module Gentzen where

import List (delete, intersect)
import Maybe (isJust, fromJust)

data Prop = Atom String |
            Conj Prop Prop |
            Disj Prop Prop |
            Impl Prop Prop |
            Neg Prop |        -- Note: This should only be used in G3cp. In G3/4ip, use p->F instead of Neg p
            T |
            F |
            Box Prop |       -- box = necessity
            Dia Prop         -- diamond = possibility
            deriving (Eq,Ord)

isAtom (Atom _) = True
isAtom _ = False
-- note: F is not an atom

instance Num Prop where
    p + q = Disj p q
    p * q = Conj p q
    negate p = Neg p -- Impl p F

infixr 4 -->

p --> q = Impl p q

a = Atom "a"
b = Atom "b"
c = Atom "c"
p = Atom "p"
q = Atom "q"
r = Atom "r"

instance Show Prop where
    show (Atom p) = p
    show (Conj p q) = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (Disj p q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (Neg p) = "~" ++ show p
    show T = "T"
    show F = "F"
    show (Box p) = "[]" ++ show p
    show (Dia p) = "<>" ++ show p


-- from Wadler, Call-by-value is dual to call-by-name
-- but Wadler distinguishes terms and coterms, with dual of a term being a coterm
dual (Atom p) = Atom p
dual (Conj p q) = Disj (dual p) (dual q)
dual (Disj p q) = Conj (dual p) (dual q)
dual (Neg p) = Neg (dual p)

-- data Sequent = Seq [Prop] [Prop]
data Sequent a = Seq [a] [a]

-- instance Show Sequent where
instance Show a => Show (Sequent a) where
    show (Seq ps qs) = " " ++ unbracket (show ps) ++ " |- " ++ unbracket (show qs) ++ " "
        where unbracket cs = init $ tail cs  -- remove the "[" and "]"

ps |- qs = Seq ps qs

data Rule = Axiom | LeftConj | RightConj | LeftDisj | RightDisj | LeftImpl | RightImpl |
            LeftImplAtom | LeftImplConj | LeftImplDisj | LeftImplImpl |
            LeftFalse | LeftNeg | RightNeg |
            LeftBox | RightBox | LeftDia | RightDia
            deriving Show

-- data ProofTree = PT Sequent Rule [ProofTree]
data ProofTree a = PT (Sequent a) Rule [ProofTree a]

-- instance Show ProofTree where
instance Show a => Show (ProofTree a) where
    show tree = showtree 0 tree
        where showtree i (PT sequent rule children) = concatMap (showtree (i+2)) children
                                                      ++ replicate i ' ' ++ show sequent ++ "    (" ++ show rule ++ ")\n"

(f +++ g) x = f x ++ g x


-- INTUITIONISTIC LOGIC
-- G3ip and G4ip - Troelstra, Schwichtenberg p77-8, p112
-- Dyckhoff - http://www.dcs.st-and.ac.uk/~rd/publications/jsl57.pdf

axiomG3ip (Seq as [p]) | isAtom p && p `elem` as = [([],Axiom)]
                       | otherwise               = []
-- note: F is not an atom, so p /= F - but we could have F `elem` as

leftFalseG3ip (Seq as [p]) | F `elem` as = [([],LeftFalse)]
                           | otherwise   = []

leftConjG3ip (Seq as [r]) = zip [ [Seq (p : q : delete f as) [r]] | f@(Conj p q) <- as] (repeat LeftConj)
-- identical to G3cp except restriction of RHS to single formula

rightConjG3ip (Seq as [Conj p q]) = [ ( [Seq as [p], Seq as [q]], RightConj) ]
rightConjG3ip _ = []

leftDisjG3ip (Seq as [r]) = zip [ let as' = delete f as in [Seq (p:as') [r], Seq (q:as') [r]] | f@(Disj p q) <- as] (repeat LeftDisj)
-- identical to G3cp except restriction of RHS to single formula

rightDisjG3ip (Seq as [Disj p q]) = zip [ [Seq as [p]], [Seq as [q]] ] (repeat RightDisj)
rightDisjG3ip _ = []

-- G3ip version of left implication rule
leftImplG3ip (Seq as [r]) = zip [ [Seq as [p], Seq (q: delete f as) [r]] | f@(Impl p q) <- as] (repeat LeftImpl)

rightImplG3ip (Seq as [Impl p q]) = [ ( [Seq (p:as) [q]], RightImpl) ]
rightImplG3ip _ = []

tacticsG3ip = axiomG3ip +++ leftConjG3ip +++ rightConjG3ip +++ leftDisjG3ip +++ rightDisjG3ip +++ leftImplG3ip +++ rightImplG3ip
              +++ leftFalseG3ip

-- G4ip replaces the left implication with four new rules
leftImplAtomG4ip (Seq as [r]) = zip [ [Seq (q : delete f as) [r]] | f@(Impl p@(Atom _) q) <- as, p `elem` as] (repeat LeftImplAtom)

leftImplConjG4ip (Seq as [e]) = zip [ [Seq ((Impl c (Impl d b)) : delete f as) [e]] | f@(Impl (Conj c d) b) <- as] (repeat LeftImplConj)
-- replace (c&d)->b by c->(d->b)

leftImplDisjG4ip (Seq as [e]) = zip [ [Seq (Impl c b : Impl d b : delete f as) [e]] | f@(Impl (Disj c d) b) <- as] (repeat LeftImplDisj)
-- replace (c|d)->b by c->b, d->b

leftImplImplG4ip (Seq as [e]) = zip [ let as' = delete f as in [Seq (Impl d b : as') [Impl c d], Seq (b : as') [e]] | f@(Impl (Impl c d) b) <- as] (repeat LeftImplImpl)

tacticsG4ip = axiomG3ip +++ leftConjG3ip +++ rightConjG3ip +++ leftDisjG3ip +++ rightDisjG3ip +++ rightImplG3ip +++
              leftImplAtomG4ip +++ leftImplConjG4ip +++ leftImplDisjG4ip +++ leftImplImplG4ip
              +++ leftFalseG3ip

proof tactics sequent =
    let subtrees = [(subtree,rule) | (subgoal,rule) <- tactics sequent, subtree <- [map (proof tactics) subgoal], all isJust subtree]
    in if null subtrees
       then Nothing
       else let (subtree,rule) = head subtrees in Just (PT sequent rule (map fromJust $ subtree))

proofG4ip = proof tacticsG4ip

isProvableG4ip sequent = isJust $ proofG4ip sequent

-- depth limited proof search
proofDL tactics depth sequent =
    if depth == 0
    then Nothing
    else let subtrees = [(subtree,rule) | (subgoal,rule) <- tactics sequent, subtree <- [map (proofDL tactics (depth-1)) subgoal], all isJust subtree]
         in if null subtrees
            then Nothing
            else let (subtree,rule) = head subtrees in Just (PT sequent rule (map fromJust $ subtree))

-- iterative deepening proof search
proofID tactics maxdepth sequent =
    let proofs = filter isJust [proofDL tactics depth sequent | depth <- [1..maxdepth] ]
    in if null proofs then Nothing else head proofs

proofG3ip = proofID tacticsG3ip


-- CLASSICAL LOGIC
-- G3cp - Troelstra & Schwichtenberg p77

axiomG3cp (Seq ps qs) | (not . null) (filter isAtom ps `intersect` filter isAtom qs) = [([],Axiom)]
                      | otherwise                                                    = []

leftFalseG3cp (Seq ps qs) | F `elem` ps = [([],LeftFalse)]
                          | otherwise   = []

leftConjG3cp (Seq ps qs) = zip [ [Seq (a : b : delete f ps) qs] | f@(Conj a b) <- ps] (repeat LeftConj)

rightConjG3cp (Seq ps qs) = zip [ let qs' = delete f qs in [Seq ps (a : qs'), Seq ps (b : qs')] | f@(Conj a b) <- qs ] (repeat RightConj)

leftDisjG3cp (Seq ps qs) = zip [let ps' = delete f ps in [Seq (a:ps') qs, Seq (b:ps') qs] | f@(Disj a b) <- ps] (repeat LeftDisj)

rightDisjG3cp (Seq ps qs) = zip [ [Seq ps (a : b : delete f qs)] | f@(Disj a b) <- qs] (repeat RightDisj)

leftImplG3cp (Seq ps qs) = zip [ [Seq (delete f ps) (a:qs), Seq (b:ps) qs] | f@(Impl a b) <- ps] (repeat LeftImpl)

rightImplG3cp (Seq ps qs) = zip [ [Seq (a:ps) (b : delete f qs)] | f@(Impl a b) <- qs] (repeat RightImpl)

leftNegG3cp (Seq ps qs) = zip [ [Seq (delete f ps) (a:qs)] | f@(Neg a) <- ps] (repeat LeftNeg)

rightNegG3cp (Seq ps qs) = zip [ [Seq (a:ps) (delete f qs)] | f@(Neg a) <- qs] (repeat RightNeg)

tacticsG3cp = axiomG3cp +++ leftConjG3cp +++ rightConjG3cp +++ leftDisjG3cp +++ rightDisjG3cp +++ leftImplG3cp +++ rightImplG3cp
              +++ leftFalseG3cp +++ leftNegG3cp +++ rightNegG3cp

proofG3cp = proofID tacticsG3cp


-- MODAL LOGIC

-- Troelstra & Schwichtenberg p287
-- G3s system for classical modal logic S4

isBox (Box _) = True
isBox _ = False

isDia (Dia _) = True
isDia _ = False

leftBoxS4 (Seq ps qs) = zip [ [Seq (a:ps) qs] | Box a <- ps] (repeat LeftBox)

rightBoxS4 (Seq ps qs) = zip [ [Seq (filter isBox ps) (a : filter isDia qs)] | Box a <- qs] (repeat RightBox)

leftDiaS4 (Seq ps qs) = zip [ [Seq (a : filter isBox ps) (filter isDia qs)] | Dia a <- ps] (repeat LeftDia)

rightDiaS4 (Seq ps qs) = zip [ [Seq ps (a:qs)] | Dia a <- qs] (repeat RightDia)

tacticsS4 = axiomG3cp +++ leftConjG3cp +++ rightConjG3cp +++ leftDisjG3cp +++ rightDisjG3cp +++ leftImplG3cp +++ rightImplG3cp
           +++ leftBoxS4 +++ rightBoxS4 +++ leftDiaS4 +++ rightDiaS4

proofS4 = proofID tacticsS4
proofS4' = proofDL tacticsS4


-- Bierman, de Paiva, On an Intuitionistic Modal Logic
-- www.cs.bham.ac.uk/~vdp/publications/studia.ps.gz

{-
axiomIS4 (Seq as [p]) | p `elem` as = [([],Axiom)]
                      | otherwise   = []
-- we have dropped the requirement that p be an atom
-}

leftImplIS4 (Seq as [r]) = zip [ let as' = delete f as in [Seq as' [p], Seq (q: as') [r]] | f@(Impl p q) <- as] (repeat LeftImpl)

leftBoxIS4 (Seq as [q]) = zip [ [Seq (p : delete f as) [q]] | f@(Box p) <- as] (repeat LeftBox)

rightBoxIS4 (Seq as [Box p]) = [ ([Seq (filter isBox as) [p]], RightBox) ] -- zip [ [Seq (filter isBox as) [p]] ] (repeat RightBox)
rightBoxIS4 _ = []

leftDiaIS4 (Seq as [Dia q]) = zip [ [Seq (p : filter isBox as) [Dia q]] | f@(Dia p) <- as] (repeat LeftDia)
leftDiaIS4 _ = []

rightDiaIS4 (Seq as [Dia p]) = [ ([Seq as [p]], RightDia) ] -- zip [ [Seq as [p]] ] (repeat RightDia)
rightDiaIS4 _ = []

tacticsIS4 = axiomG3ip +++ leftConjG3ip +++ rightConjG3ip +++ leftDisjG3ip +++ rightDisjG3ip +++ leftImplIS4 +++ rightImplG3ip
           +++ leftBoxIS4 +++ rightBoxIS4 +++ leftDiaIS4 +++ rightDiaIS4

proofIS4 = proof tacticsIS4


-- TESTS

-- types of some standard combinators
typeI = a --> a -- type of I == \x -> x
typeK = a --> (b --> a) -- type of K == \x y -> x
typeS = (a --> (b --> c)) --> ( (a --> b) --> (a --> c) ) -- type of S == \x y z -> x z (y z)
typeB = (a --> b) --> ((c --> a) --> (c --> b)) -- type of B == \x y z -> x (y z)
typeB' = (a --> b) --> ((b --> c) --> (a --> c)) -- type of B' == \x y z -> y (x z)
typeC = (a --> (b --> c)) --> (b --> (a --> c)) -- type of C == \x y z -> x z y
typeW = (a --> (a --> b)) --> (a --> b) -- type of W == \x y -> x y y
type0 = a --> (b --> b) -- type of 0 == \x y -> y
type1 = (a --> b) --> (a --> b) -- type of 1 == \x y -> x y

commuteAnd = a*b --> b*a
commuteOr  = a+b --> b+a

assocAnd = a*(b*c) --> (a*b)*c
assocOr  = a+(b+c) --> (a+b)+c

distrL   = (a+b)*c --> a*c+b*c
undistrL = a*c+b*c --> (a+b)*c
distrR   = a*(b+c) --> a*b+a*c
undistrR = a*b+a*c --> a*(b+c)

tcurry = (a*b --> c) --> a --> b --> c
tuncurry = (a --> b --> c) --> a*b --> c

pierce'sLaw = ((p --> q) --> p) --> p
-- provable in classical but not in intuitionistic logic

-- Theorems of IS4
{-
Box (a-->b) --> (Box a --> Box b)
Box a --> a
Box a --> Box (Box a)
Box (a --> Dia b) --> (Dia a --> Dia b)
a --> Dia a
-}