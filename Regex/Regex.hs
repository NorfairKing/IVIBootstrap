{-# LANGUAGE GADTs #-}

module Regex.Regex where

import Data.List (nub)


data RE a where
    Phi :: RE a                     -- empty language
    Empty :: RE a                   -- empty word
    L :: a -> RE a                  -- single letter taken from alphabet a
    Choice :: RE a -> RE a -> RE a  -- r1 + r2
    Seq :: RE a -> RE a -> RE a     -- (r1,r2)
    Star :: RE a -> RE a            -- r*
    Var :: Int -> RE a              -- first-order variables to represent regular equations
    deriving Eq

    
-- Print a regex (in the symbolic manner)
instance Show a => Show (RE a) where
    show Phi = "{}"
    show Empty = "<>"
    show (L c) = show c
    show (Choice r1 r2) = ("(" ++ show r1 ++ "|" ++ show r2 ++ ")")
    show (Seq r1 r2) = ("<" ++ show r1 ++ "," ++ show r2 ++ ">")
    show (Star r) = (show r ++ "*") 

-- Make a disjunction of regexes
resToRE :: [RE a] -> RE a
resToRE (r:res) = foldl Choice r res
resToRE [] = Phi

-- Compute the alphabet of a regex
sigmaRE :: Eq a => RE a -> [a]
sigmaRE (L l) = [l]
sigmaRE (Seq r1 r2) = nub ((sigmaRE r1) ++ (sigmaRE r2))
sigmaRE (Choice r1 r2) = nub ((sigmaRE r1) ++ (sigmaRE r2))
sigmaRE (Star r) = sigmaRE r
sigmaRE Phi = []
sigmaRE Empty = []

-- Test wether a regex is empty (empty word)
isEmpty :: RE a -> Bool
isEmpty Phi = False
isEmpty Empty = True
isEmpty (Choice r1 r2) = (isEmpty r1) || (isEmpty r2)
isEmpty (Seq r1 r2) = (isEmpty r1) && (isEmpty r2)
isEmpty (Star r) = True
isEmpty (L _) = False

-- Test if a regex contains nothing
isPhi :: RE a -> Bool
isPhi Phi = True
isPhi Empty = False
isPhi (Choice r1 r2) = (isPhi r1) && (isPhi r2)
isPhi (Seq r1 r2) = (isPhi r1) || (isPhi r2)
isPhi (Star r) = False
isPhi (L _) = False

{-
    Brozozowski's derivative operation
    deriv r l denotes the regular expression where the
    "leading l has been removed"
    (not necessary for the intersection algorithm,
    only included to illustrate the difference
    to the upcoming partial derivative algorithm)
-}
deriv :: Eq a => RE a -> a -> RE a
deriv Phi _ = Phi
deriv Empty _ = Phi
deriv (L l1) l2
    | l1 == l2  = Empty
    | otherwise = Phi
deriv (Choice r1 r2) l =
    Choice (deriv r1 l) (deriv r2 l)
deriv (Seq r1 r2) l =
    if isEmpty r1
    then Choice (Seq (deriv r1 l) r2) (deriv r2 l)
    else Seq (deriv r1 l) r2
deriv (this@(Star r)) l =
    Seq (deriv r l) this


{-
    (a variant) of Antimirov's partial derivative operation

    Antimirov demands that partDeriv (Star (L 'A')) 'A' yields [A*]
    whereas our version yields [<<>,'A'*>].
    The difference is not essential here.
-}

partDeriv :: Eq a => RE a -> a -> [RE a]
partDeriv Phi l = []
partDeriv Empty l = []
partDeriv (L l') l
    | l == l'   = [Empty]
    | otherwise = []
partDeriv (Choice r1 r2) l = nub ((partDeriv r1 l) ++ (partDeriv r2 l))
partDeriv (Seq r1 r2) l
    | isEmpty r1 =
        let s1 = [ (Seq r1' r2) | r1' <- partDeriv r1 l ]
            s2 = partDeriv r2 l
        in nub (s1 ++ s2)
    | otherwise = [ (Seq r1' r2) | r1' <- partDeriv r1 l ]
partDeriv (Star r) l = [ (Seq r' (Star r)) | r' <- partDeriv r l ]


type Env a = [((RE a, RE a), RE a)]

{-
    Converting a regular equation into a regular expression

    We assume that variables x appears (if at all) at position (r,Var x)
    convert2 traveres the regexp and yields (r1,r2)
    where the invariant is that r1 is part of the loop and r2 is the base case
-}
convert :: Int -> RE a -> RE a
convert x r = let (r1,r2) = convert2 x r
              in Seq (Star r1) r2

convert2 :: Int -> RE a -> (RE a, RE a)
convert2 x Empty = (Empty, Empty)
convert2 x (Var y)
       | x == y    = (Empty,Phi)
       | otherwise = (Empty, Var y) -- can this happen?
convert2 x (r@(Seq l r1))
       | mentions x r1 = let (r2,r3) = convert2 x r1
                         in (Seq l r2, r3)
       | otherwise = (Empty, r)
convert2 x (r@(L _)) = (Empty, r)
convert2 x (Choice r1 r2) = let (r1', r1'') = convert2 x r1
                                (r2', r2'') = convert2 x r2
                            in (Choice r1' r2', Choice r1'' r2'')

mentions :: Int -> RE a -> Bool
mentions x (Var y) = x == y
mentions x (Seq r1 r2) = mentions x r1 || mentions x r2
mentions x (Star r) = mentions x r
mentions x (Choice r1 r2) = mentions x r1 || mentions x r2
mentions x _ = False


-- Here's the successful attempt of the partial derivative based intersection algorithm.

intersect       :: Eq a => RE a -> RE a -> RE a
intersect r1 r2 = intersectC 1 [] r1 r2

intersectC      :: Eq a => Int -> Env a -> RE a -> RE a -> RE a
intersectC cnt env r1 r2
    | r1 == Phi || r2 == Phi = Phi
    | r1 == Empty = if isEmpty r2
                    then Empty
                    else Phi
    | r2 == Empty = if isEmpty r1
                    then Empty
                    else Phi
    | otherwise =
        case lookup (r1,r2) env of
        Just r -> r
        Nothing ->
            let letters = sigmaRE (r1 `Choice` r2)
                env' = ((r1,r2),Var cnt):env
                r1l l = resToRE $ partDeriv r1 l
                r2l l = resToRE $ partDeriv r2 l
                r' = resToRE $ map (\l -> Seq (L l) (intersectC (cnt+1) env' (r1l l) (r2l l))) letters
                r =  if (isEmpty r1) && (isEmpty r2)
                     then Choice r' Empty
                    else r'
            in convert cnt r

{-
    For testing purposes, it's handy to have a function which tests for (semantic)
    equality among regular expressions (again written using partial derivatives).
-}
type EnvEq a = [(RE a, RE a)]

equality :: Eq a => RE a -> RE a -> Bool
equality r1 r2 = eqREC [] r1 r2

eqREC :: Eq a => EnvEq a -> RE a -> RE a -> Bool
eqREC env r1 r2
  | isEmpty r1 && (not (isEmpty r2)) = False
  | isPhi r1 && (not (isPhi r2)) = False
  | otherwise =
     if elem (r1,r2) env
     then True
     else let letters = sigmaRE (r1 `Choice` r2)
              env' = (r1,r2):env
              r1l l = resToRE $ partDeriv r1 l
              r2l l = resToRE $ partDeriv r2 l
          in and $ map (\l -> eqREC env' (r1l l) (r2l l)) letters


containsRECheap :: Eq a => RE a -> RE a -> Bool
containsRECheap r1 r2 = equality r1 (intersect r1 r2)


contains :: Eq a => RE a -> RE a -> Bool
contains r1 r2 = containsC [] r1 r2

containsC :: Eq a => EnvEq a -> RE a -> RE a -> Bool
containsC env r1 r2
  | r1 == Empty = isEmpty r2
  | r1 == Phi = True
  | r2 == Phi = equality r1 Phi       -- I think r1 == Phi should be fine
  | r2 == Empty = equality r1 Empty   -- same here
  | elem (r1,r2) env = True
  | otherwise =
        let letters = sigmaRE (r1 `Choice` r2)
            env' = (r1,r2) :env
            r1l l = resToRE $ partDeriv r1 l
            r2l l = resToRE $ partDeriv r2 l
            b = and $ map (\l -> containsC env' (r1l l) (r2l l)) letters
        in b && ((isEmpty r1) `implies` (isEmpty r2))


implies :: Bool -> Bool -> Bool
implies a b = (not a) || b


-- UNFINISHED
regex       :: String -> RE Char
regex ""    = Empty

regex [c]   = L c


