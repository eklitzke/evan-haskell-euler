module Main where
import EulerMath
--  ghc --make -XParallelListComp -i /home/evan/euler/common/euler_math.hs euler_problem.hs

nums = filter even (fibs_to 4000000)
main = putStrLn (show $ sum nums)
