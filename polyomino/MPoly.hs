-- mpoly.hs

module MPoly (MPoly (..), TermOrder (..), VarSet(..),
              x, y, z, s, t, u, v, w, a, b, c, d, constMP,
              x0, x1, x2, x3, x_, monomial,
			  paramsMP, termsMP, termOrderMP, varsMP,
              toMonicMP, degMP,
              changeTermOrder,
			  quotRemMP, lt, lc, lp, ltMP,
              dividesTm, properlyDividesTm, divTm, lcmTm, gcdTm, degTm, (*/),
			  (/%), (%%), isReducibleMP,
			  toHomogeneous, fromHomogeneous,
			  evalMP, substMP) where

import List(intersperse)
import QQ
import FF
import MergeSort
import MathsPrimitives ( ($+), ($-), ($.) )


infix 5 /%, %%

-- MULTIVARIATE POLYNOMIALS

data TermOrder = Lex | Glex | Grevlex | Elim Int | Elimv [Int] deriving (Eq, Show)

data VarSet = Xyz | Xi deriving (Eq, Show)

data MPoly a = MP (TermOrder, VarSet) [Term a]


-- ACCESSOR FUNCTIONS

paramsMP (MP params _) = params

termsMP (MP _ terms) = terms

termOrderMP (MP (termOrder, _) _) = termOrder

varsMP (MP (_, vars) _) = vars


-- PRE-DEFINED VARIABLES

x = MP (Grevlex, Xyz) [(Q 1 1, [1])]
y = MP (Grevlex, Xyz) [(Q 1 1, [0,1])]
z = MP (Grevlex, Xyz) [(Q 1 1, [0,0,1])]
s = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,1])]
t = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,1])]
u = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,0,1])]
v = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,0,0,1])]
w = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,0,0,0,1])]
a = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,0,0,0,0,1])]
b = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,0,0,0,0,0,1])]
c = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,0,0,0,0,0,0,1])]
d = MP (Grevlex, Xyz) [(Q 1 1, [0,0,0,0,0,0,0,0,0,0,0,1])]

constMP c = MP (Grevlex, Xyz) [(c,[])]

-- Normally use x1.., and reserve x0 for dealing with homogeneous case
-- xvar i = MP Grevlex Xi [(Q 1 1, replicate i 0 ++ [1])]
x_ i = MP (Grevlex, Xi) [(Q 1 1, replicate i 0 ++ [1])]
x0 = MP (Grevlex, Xi) [(Q 1 1, [1])]
x1 = MP (Grevlex, Xi) [(Q 1 1, [0,1])]
x2 = MP (Grevlex, Xi) [(Q 1 1, [0,0,1])]
x3 = MP (Grevlex, Xi) [(Q 1 1, [0,0,0,1])]

monomial as = MP (Grevlex, Xi) [(Q 1 1, 0:as)]


-- POWER PRODUCTS
-- for example x^3 y^2 is represented as [3,2]

type PowerProduct = [Int]

-- we allow PP lists of unequal lengths, so need our own version of (==)
eqPP as bs = all (==0) (as $- bs)

multPP as bs = as $+ bs

dividesPP as bs = all (>=0) (bs $- as)

divPP as bs = as $- bs

gcdPP as [] = []
gcdPP [] bs = []
gcdPP (a:as) (b:bs) = min a b : gcdPP as bs

lcmPP as [] = as
lcmPP [] bs = bs
lcmPP (a:as) (b:bs) = max a b : lcmPP as bs


-- TERMS
-- for example, 3xy is represented as (3,[1,1])

-- !! It might be better to go directly to power products, and reduce our use of terms

type Term a = (a, PowerProduct)

eqTm (c,as) (d,bs) = c == d && as `eqPP` bs

multTm (c,as) (d,bs) = (c*d, multPP as bs)

dividesTm (_,as) (_,bs) = dividesPP as bs

-- properlyDividesTm t@(_,as) u@(_,bs) = dividesTm t u && sum as < sum bs
properlyDividesTm (_,as) (_,bs) = dividesPP as bs && not (eqPP as bs)

divTm (c,as) (d,bs) = (c/d, divPP as bs)

lcmTm (c,as) (d,bs) = (c*d, lcmPP as bs) -- !! the c*d here is arbitrary

gcdTm (c,as) (_,bs) = (c/c, gcdPP as bs)

degTm (_,as) = sum as


-- TERM ORDERINGS

isGraded Grevlex = True
isGraded Glex = True
isGraded _ = False



type TermOrdering a = Term a -> Term a -> Bool

-- in practice, term orderings always ignore the coefficients and only use the PowerProduct

lexgt (_,as) (_,bs) =
	let ds = dropWhile (==0) (as $- bs)
	in if null ds then False else head ds > 0
-- ie first non-zero difference is positive

revlexgt (_,as) (_,bs) =
	let ds = dropWhile (==0) (reverse (as $- bs))
	in if null ds then False else head ds < 0
-- ie last non-zero difference is negative

-- convert an ungraded term ordering into a graded term ordering
gradedgt :: TermOrdering a -> TermOrdering a
gradedgt termordgt t u =
	degt > degu ||
	(degt == degu && termordgt t u)
	where
		degt = degTm t
		degu = degTm u

glexgt :: TermOrdering a
glexgt = gradedgt lexgt

grevlexgt :: TermOrdering a
grevlexgt = gradedgt revlexgt

-- Elimination order - Cox et al, Ideals, Varieties and Algorithms
elim l t@(_,as) u@(_,bs) = sumlas > sumlbs || (sumlas == sumlbs && grevlexgt t u)
	where
		sumlas = sum (take l as)
		sumlbs = sum (take l bs)

elimv vs t@(_,as) u@(_,bs) = sumvas > sumvbs || (sumvas == sumvbs && grevlexgt t u)
	where
		sumvas = vs $. as
		sumvbs = vs $. bs

-- collectTerms :: TermOrdering -> [Term] -> [Term]
collectTerms order f = doCollectTerms (mergeSort' order f)
	where
		doCollectTerms [] = []
		doCollectTerms (t@(c,as):[])
			| c == 0    = []
			| otherwise = t:[]
		doCollectTerms (t1@(c,as):t2@(d,bs):ts)
			| c == 0        = doCollectTerms (t2:ts)
			| as `eqPP` bs  = doCollectTerms ((c+d,as):ts)
			| otherwise     = t1 : doCollectTerms (t2:ts)



termOrderToFn to = case to of
	Lex     -> lexgt
	Glex    -> glexgt
	Grevlex -> grevlexgt
	Elim l  -> elim l
	Elimv vs -> elimv vs

termOrderFn f = termOrderToFn (termOrderMP f)


-- EQ INSTANCE

instance Eq a => Eq (MPoly a) where
    MP _ [] == MP _ [] = True
    MP _ [(c,[])] == MP _ [(d,[])] = c == d
    MP params ts == MP params' ts' = params == params' && length ts == length ts' && and (zipWith eqTm ts ts')

-- ORD INSTANCE

instance (Eq a, Ord a) => Ord (MPoly a) where
	compare f g
		| paramsMP f /= paramsMP g = error "MPoly.compare: parameter mismatch"
		| otherwise = doCompare (termOrderFn f) (termsMP f) (termsMP g)

doCompare _ [] [] = EQ
doCompare _ _ [] = GT
doCompare _ [] _ = LT
doCompare termord (t@(c,as):ts) (u@(d,bs):us) =
	if eqPP as bs
	then case compare c d of
		LT -> LT
		GT -> GT
		EQ -> doCompare termord ts us
	else if termord t u then GT else LT


-- SHOW INSTANCE

instance (Show a, Num a) => Show (MPoly a) where
	show f =
		let vars = case varsMP f of
			Xyz -> ["x", "y", "z", "s", "t", "u", "v", "w", "a", "b", "c", "d"] ++ map (:[]) ['e'..'r']
			Xi  -> ["x" ++ show i | i <- [0..]]
		in showTerms vars (termsMP f)

-- showTerms vars ts = concat (intersperse "+" (map (showTerm vars) ts))
showTerms _ [] = "0"
showTerms vars ts = foldl1 linkTerms (map (showTerm vars) ts)
	where linkTerms t u = if head u == '-' then t ++ u else t ++ "+" ++ u

showTerm vars (c,as) = showCoeff c as ++ showPP vars as

showCoeff c as =
	let c' = show c in case c' of
	"1"       -> if any (/=0) as then "" else "1"
	"-1"      -> if any (/=0) as then "-" else "-1"
	otherwise -> c'

showPP vars indices = concat (map showPower (zip vars indices))
	where
		showPower (var,index)
			| index == 0  = ""
			| index == 1  = var
			| otherwise   = var ++ '^': show index


changeTermOrder to (MP (_, vs) ts) = MP (to, vs) ts'
	where ts' = collectTerms (termOrderToFn to) ts


-- RING OPERATIONS FOR MULTIVARIATE POLYNOMIALS

instance Num a => Num (MPoly a) where
    MP params@(to,_) f + MP params' g | params == params' = MP params (addMP tofn f g)
        where tofn = termOrderToFn to
    f@(MP params _) + MP _ g@[(_,[])] = f + MP params g -- if one of the summands is a constant, ignore parameter mismatch 
    MP _ f@[(_,[])] + g@(MP params _) = MP params f + g
    f + MP _ [] = f -- if one of the summands is zero, ignore parameter mismatch 
    MP _ [] + g = g

    negate (MP params f) = MP params (negateMP f)

    MP params@(to,_) f * MP params' g | params == params' = MP params (multMP tofn f g)
        where tofn = termOrderToFn to
    f@(MP params _) * MP _ g@[(_,[])] = f * MP params g -- if one of the summands is a constant, ignore parameter mismatch 
    MP _ f@[(_,[])] * g@(MP params _) = MP params f * g
    _ * (MP _ []) = 0
    (MP _ []) * _ = 0

    fromInteger 0 = MP (Grevlex, Xyz) []
    fromInteger n = MP (Grevlex, Xyz) [(fromInteger n, [])]
    -- NOTE: fromInteger gives you Grevlex over Q.
    -- If you want something else, use the conversion functions, or construct by hand

-- convert an mpoly from rationals to a finite field
(/%) :: MPoly QQ -> Integer -> MPoly FF
MP params ts /% p = MP params ts''
	where
		ts' = map (\(Q n 1, as) -> (toFp p n, as)) ts
		ts'' = filter (\(F _ x, _) -> x /= 0) ts'

addMP _ f [] = f
addMP _ [] f = f
addMP termord (t@(c,as):ts) (u@(d,bs):us) =
	if as `eqPP` bs
	then
		if c+d == 0
		then addMP termord ts us
		else (c+d,as) : (addMP termord ts us)
	else
		if termord t u
		then t : (addMP termord ts (u:us))
		else u : (addMP termord (t:ts) us)

negateMP f = map (\(c,as) -> (-c,as)) f

subMP termord f g = addMP termord f (negateMP g)

multMP _ _ [] = []
multMP _ [] _ = []
multMP termord (t:ts) (u:us) =
	let
		hh = [multTm t u]
		ht = map (multTm t) us
		th = map (multTm u) ts
		tt = multMP termord ts us
	in addMP termord (addMP termord hh tt) (addMP termord ht th)
-- !! experiment with addition orders. hh will be largest term, so will add in one step


instance Fractional a => Fractional (MPoly a) where
    recip (MP params [(c,[])]) = MP params [(recip c,[])]
    recip _ = error "recip not defined for non-constant MPoly"


toMonicMP f@(MP _ []) = f
toMonicMP (MP params ts@((k,_):_)) = MP params (map (\(c,as)->(c/k,as)) ts)


-- DIVISION ALGORITHM


-- initial term
ltMP (MP params (t:_)) = MP params [t]

lt f
	| null ts = error "lt 0"
	| otherwise = head ts
	where ts = termsMP f

ltSplit (MP params (t:ts)) = (MP params [t], MP params ts)

lc f = let (c,_) = lt f in c

lp f = let (_,as) = lt f in as

divideslt f g = dividesTm (lt f) (lt g)

divlt (MP params (t:_)) (MP _ (u:_)) = MP params [divTm t u]

degMP (MP (to, _) ts)
	| null ts     = -1
	| isGraded to = degTm (head ts)
	| otherwise   = foldl max 0 (map degTm ts)


t */ MP params ts = MP params (map (multTm t) ts)


quotRemMP f@(MP (termord, vs) _) gs = doQuotRemMP f (replicate (length gs) zero, zero)
	 where
		zero = MP (termord, vs) [] -- slight kludge
		doQuotRemMP h (us,r)
			| null (termsMP h)  = (us,r)
			| otherwise       = doDivisionStep h (gs,[],us,r)
		doDivisionStep h ([],us',[],r) =
			let (lth,h') = ltSplit h
			in doQuotRemMP h' (reverse us', r + lth)
			-- let lth = ltMP h
			-- in doQuotRemMP (h - lth) (reverse us', r + lth)
		doDivisionStep h (g:gs,us',u:us,r) =
			if divideslt g h
			then
				let
					newTerm = divlt h g
					u' = u + newTerm
					h' = h - (newTerm * g)
				in doQuotRemMP h' (reverse us' ++ (u' : us), r)
			else doDivisionStep h (gs,u:us',us,r)


reduceMP f gs = r where (_,r) = quotRemMP f gs


(%%) :: (Num a, Fractional a) => MPoly a -> [MPoly a] -> MPoly a
(%%) = reduceMP

isReducibleMP f gs = or [lt g `dividesTm` lt f | g <- gs]


-- evaluate the mpoly at the point passed in - eg evalMP (x^2+y) [1,2]
evalMP f vs = sum (map (\t -> evalTerm t vs) (termsMP f))
	where evalTerm (c,as) vs = foldl (*) c (zipWith (^) vs as)

-- substitute expressions for the vars - eg substMP (x1x2) [1,x+y,x^2+y^2] - (note we need to pass an expression for x0 too)
substMP f vs = sum (map (\t -> evalTerm t vs) (termsMP f))
	where evalTerm (c,as) vs = foldl (*) (constMP c) (zipWith (^) vs as)
-- (Could perhaps be called composeMP instead)

-- NOTE: The following two functions don't individually preserve variable names (though fromHomogeneous . toHomogeneous does)
toHomogeneous f@(MP params ts) =
	let
		d = degMP f
		ts' = collectTerms (termOrderFn f) (map (\(c,as) -> (c, d - sum as : as)) ts)
	in MP params ts'

fromHomogeneous f@(MP params ts) =
	let ts' = collectTerms (termOrderFn f) (map (\(c,as) -> if null as then (c,[]) else (c, tail as)) ts)
	in MP params ts'


-- Will also provide two functions toHomogeneousUsing, fromHomogeneousUsing, where you can specify the variable to be used

{-
isHomogeneousMP f = case termsMP f of
	[] -> True
	(t:ts) -> all (== degTm t) (map degTm ts)

-}