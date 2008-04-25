-- permutationgroupexamples.hs

module PermutationGroupExamples where

import RedBlackTree
import PermutationGroups
import PermutationOrbits
import SchreierSims


-- AUTOMORPHISM GROUPS OF SOME GRAPHS AND SET SYSTEMS

-- Automorphism group of (graph of) cube
cubeGraph = toGraph [(1,2),(1,3),(2,4),(3,4),(1,5),(2,6),(3,7),(4,8),(5,6),(5,7),(6,8),(7,8)]

cubeFaces = toSetSystem [ [1,2,3,4], [5,6,7,8], [1,3,5,7], [1,2,5,6], [2,4,6,8], [3,4,7,8] ]

alphaCube = fromCycles [[1,2,4,8,7,5],[3,6]] -- 60 degree rotation about the line 3-6, followed by reflection in the midplane between 3-6
betaCube = fromCycles [[1,2,4,3],[5,6,8,7]]  -- 90 degree rotation about the line joining the centres of the front and back faces

schreierSimsCube = schreierSimsTransversals 8 [alphaCube, betaCube]

cubeRotations = schreierSimsTransversals 8 [alphaCube^2, betaCube]


-- Cameron, Combinatorics: Topics, Techniques, Algorithms, p240, 243
petersenGraph = toGraph [(1,2),(2,3),(3,4),(4,5),(5,1),(6,8),(7,9),(8,10),(9,6),(10,7),(1,6),(2,7),(3,8),(4,9),(5,10)]

alphaPetersen = fromCycles [[1,2,3,4,5],[6,7,8,9,10]]
betaPetersen = fromCycles [[1,2,3,4,9,6],[5,7,8],[10]]

petersenSS = schreierSimsTransversals 10 [alphaPetersen, betaPetersen]

-- the generated group has order 120 - it is S5


-- Cameron, Combinatorics, p239, 242
sts7 = toSetSystem [[1,2,4],[2,3,5],[3,4,6],[4,5,7],[5,6,1],[6,7,2],[7,1,3]]
-- this is the Fano plane, viewed as a Steiner Triple System on 7 points

alphaSTS7 = fromCycles [[1,2,3,4,5,6,7]]
betaSTS7 = fromCycles [[1],[2],[3,7],[4],[5,6]]

schreierSimsSTS7 = schreierSimsTransversals 7 [alphaSTS7, betaSTS7]

-- The generated group has order 168 - it is GL(3,2) - see Cameron for justification

-- should rewrite the above to count from zero, and then functions to adjust


-- THE MATHIEU GROUPS

-- L2(23)
-- Linear transformations of the projective line over F23

-- we represent the points of the projective line over Fp as [-1..p-1], where -1 represents the point at infinity
fromProjectiveLine x = x+2  -- because our permutations act on [1..n]
toProjectiveLine x = x-2


alphaL2_23 = fromCycles (map (map (+2)) [[-1],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]])                      -- t -> t+1
betaL2_23 = fromCycles (map (map (+2)) [[-1],[15,7,14,5,10,20,17,11,22,21,19],[0],[3,6,12,1,2,4,8,16,9,18,13]])                   -- t -> 2*t
gammaL2_23 = fromCycles (map (map (+2)) [[-1,0],[15,3],[7,13],[14,18],[5,9],[10,16],[20,8],[17,4],[11,2],[22,1],[21,12],[19,6]])  -- t -> -1/t

schreierSimsL2_23 = schreierSimsTransversals 24 [alphaL2_23, betaL2_23, gammaL2_23]


-- Mathieu group M24
-- Conway and Sloane p274ff
-- This is the automorphism group of the extended binary Golay code G24
-- or alternatively of the unique Steiner system S(5,8,24) (which consists of the weight 8 codewords of the above)

deltaM24 = fromCycles (map (map (+2)) [[-1],[14,17,11,19,22],[15],[20,10,7,5,21],[0],[18,4,2,6,1],[3],[8,16,13,9,12]])
-- this is t -> t^3 / 9 (for t a quadratic residue), t -> 9 t^3 (t a non-residue)

generatorsM24 = [alphaL2_23, betaL2_23, gammaL2_23, deltaM24]
-- schreierSimsM24 = schreierSimsTransversals 24 generatorsM24
schreierSimsM24 = randomSchreierSimsTransversals 24 generatorsM24 929847293
orderM24 = 244823040
-- we declare this as a variable so that once it's calculated, it stays calculated (it won't be calculated before because of lazy evaluation)


-- Steiner system S(5,8,24)

octad = toBlock (map (+2) [0,1,2,3,4,7,10,12])
-- Conway&Sloane p276 - this is a weight 8 codeword from Golay code G24

s_5_8_24 = orbit [alphaL2_23, betaL2_23, gammaL2_23] octad
-- S(5,8,24) constructed as the image of a single octad under the action of PSL(2,23)
-- it has 759 blocks ( (24 `choose` 5) `div` (8 `choose` 5) )
-- The automorphism group of S(5,8,24) is in fact the full Mathieu group M24
-- (You can check that deltaM24 is also an automorphism)


-- Extended binary Golay code G24

qr23 = [1,2,3,4,6,8,9,12,13,16,18] :: [Int] -- quadratic residues mod 23

dodecad = toBlock (map (+2) (0:qr23))

weight12codewordsG24 = orbit [alphaL2_23, betaL2_23, gammaL2_23, deltaM24] dodecad
-- this has 2576 elts
-- we do need the full Mathieu group to generate this - the orbit under PSL(2,23) has only 552 elts

-- we can now list G24 if desired
-- it is the zero codeword, and its complement, the weight 8 codewords and their complements, and the weight 12 codewords


-- schreierSimsM23 = onePointStabilizer schreierSimsM24
-- order 10200960

s_4_7_23 = map (\(B (1:xs)) -> B xs) (filter (\(B (x:xs)) -> x == 1) s_5_8_24)
-- 253 blocks ( (23 `choose` 4) `div` (7 `choose` 4) )


-- schreierSimsM22 = onePointStabilizer schreierSimsM23
-- order 443520


-- L2(11)
-- Linear transformations of the projective line over F11
-- Conway&Sloane 268-271

alphaL2_11 = fromCycles (map (map (+2)) [[-1],[0,1,2,3,4,5,6,7,8,9,10]])          -- t -> t+1
betaL2_11 = fromCycles (map (map (+2)) [[-1],[0],[1,3,9,5,4],[2,6,7,10,8]])       -- t -> 3t
gammaL2_11 = fromCycles (map (map (+2)) [[-1,0],[1,10],[2,5],[3,7],[4,8],[6,9]])  -- t -> - 1/t

schreierSimsL2_11 = schreierSimsTransversals 12 [alphaL2_11, betaL2_11, gammaL2_11]

-- Mathieu group M12
-- Conway and Sloane p327
-- M12 is the automorphism group of the extended ternary Golay code G12
-- or alternatively of the unique Steiner system S(5,6,12)
-- (with perhaps a cyclic group of order 2 thrown in)

deltaM12 = fromCycles (map (map (+2)) [[-1],[0],[1],[2,10],[3,4],[5,9],[6,7],[8]])

schreierSimsM12 = schreierSimsTransversals 12 [alphaL2_11, betaL2_11, gammaL2_11, deltaM12]
-- order 95040


qr11 = [1,3,4,5,9] :: [Int]

hexad = toBlock (map (+2) (0:qr11))

s_5_6_12 = orbit [alphaL2_11, betaL2_11, gammaL2_11] hexad
-- This has 132 blocks ( (12 `choose` 5) `div` (6 `choose` 5) )

-- schreierSimsM11 = onePointStabilizer schreierSimsM12
-- order 7920



-- We can also generate G24 using linear algebra in F2(24)
-- it is the code spanned by the images of the dodecad under the action of PSL(2,23)


-- RUBIK'S 3-CUBE

--             10    11    12
--             13    14    15
--             16    17    18
--  37 38 39  1(48) 2(47) 3(46)  19 20 21  (46 47 48)
--  40 41 42  4(51) 5(50) 6(49)  22 23 24  (49 50 51)
--  43 44 45  7(54) 8(53) 9(52)  25 26 27  (52 53 54)
--             28    29    30
--             31    32    33
--             34    35    36

f = fromCycles' 54 [[1,3,9,7],[2,6,8,4],[16,19,30,45],[17,22,29,42],[18,25,28,39]]           -- rotate front face
b = fromCycles' 54 [[46,48,54,52],[47,51,53,49],[10,43,36,21],[11,40,35,24],[12,37,34,27]]   -- rotate back face
l = fromCycles' 54 [[1,28,54,10],[4,31,51,13],[7,34,48,16],[37,39,45,43],[38,42,44,40]]      -- rotate left face
r = fromCycles' 54 [[3,12,52,30],[6,15,49,33],[9,18,46,36],[19,21,27,25],[20,24,26,22]]      -- rotate right face
u = fromCycles' 54 [[1,37,46,19],[2,38,47,20],[3,39,48,21],[10,12,18,16],[11,15,17,13]]      -- rotate up (top) face
d = fromCycles' 54 [[7,25,52,43],[8,26,53,44],[9,27,54,45],[28,30,36,34],[29,33,35,31]]      -- rotate down (bottom) face

f' = inverse f
b' = inverse b
l' = inverse l
r' = inverse r
u' = inverse u
d' = inverse d

generatorsRubikCube =  [f,b,l,r,u,d]
-- we keep the face centres fixed. The "supergroup" is the direct product of this group with the rotation group of the cube (of order 24)

rubikCube = randomSchreierSimsTransversals 54 generatorsRubikCube 876423876
orderRubikCube = 43252003274489856000


