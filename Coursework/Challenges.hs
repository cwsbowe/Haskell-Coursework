{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),LetExpr(..),CLExpr(..),
                   calcBBInteractions,
                   solveBB,
                   prettyPrint,
                   parseLet,
                   clTransform,
                   innerRedn1,outerRedn1,innerCLRedn1,outerCLRedn1,compareInnerOuter
                   )
where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Function (on)

instance NFData CLExpr
instance NFData LetExpr
instance NFData LamExpr 
instance NFData Marking
instance NFData Side

-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ]
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)


calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms | checkInRange n atoms = checkAllInteractions n atoms 1 [North, East, South, West]
    | otherwise = error "One or more atoms not in range of grid"


--Recursively goes through each point on each side and fires a beam into the Black Box
checkAllInteractions :: Int -> Atoms -> Int -> [Side] -> Interactions
checkAllInteractions n atoms count [] = []
checkAllInteractions n atoms count sides | count > n = checkAllInteractions n atoms 1 (tail sides)
    | otherwise = fireBeam n atoms (head sides, count) : checkAllInteractions n atoms (count+1) sides


--Fires a beam into the black box
--Mark the current position of the beam according to edge position
fireBeam :: Int -> Atoms -> EdgePos -> (EdgePos, Marking)
fireBeam n atoms edgePos | fst edgePos == North = (edgePos, checkDirection n atoms edgePos (snd edgePos, 0))
    | fst edgePos == East = (edgePos, checkDirection n atoms edgePos (n+1, snd edgePos))
    | fst edgePos == South = (edgePos, checkDirection n atoms edgePos (snd edgePos, n+1))
    | otherwise = (edgePos, checkDirection n atoms edgePos (0, snd edgePos))

--Checks if any of the atoms lie outside the black box
checkInRange :: Int -> Atoms -> Bool
checkInRange n [] = True
checkInRange n atoms | fst (head atoms) > n || snd (head atoms) > n = False
    | otherwise = checkInRange n (tail atoms)

--Checks which direction the beam is coming from and sorts the list of atoms appropriately
--Calls appropriate checking function depending on which side the beam is firing from
checkDirection :: Int -> Atoms -> EdgePos -> Pos -> Marking
checkDirection n atoms edgePos currentPos | fst edgePos == North = checkNorth n (sortBy (compare `on` snd) $ sortBy (compare `on` fst) atoms) edgePos 0 currentPos
    | fst edgePos == East = checkEast n (sortBy (flip compare `on` fst) $ sortBy (compare `on` snd) atoms) edgePos 0 currentPos
    | fst edgePos == South = checkSouth n (sortBy (flip compare `on` snd) $ sortBy (compare `on` fst) atoms) edgePos 0 currentPos
    | otherwise = checkWest n (sortBy (compare `on` fst) $ sortBy (compare `on` snd) atoms) edgePos 0 currentPos

--Checks which atom a beam fired from North interacts with
checkNorth :: Int -> Atoms -> EdgePos -> Int -> Pos -> Marking
checkNorth n atoms edgePos checkAtom currentPos | checkAtom+1 > length atoms = Path (South, snd edgePos) --If checked all atoms then we know the path
--Test    | fst (atoms!!checkAtom) == snd edgePos && snd (atoms!!checkAtom) > snd currentPos = Reflect
    | fst (atoms!!checkAtom) == snd edgePos && snd (atoms!!checkAtom) > snd currentPos = Absorb
--If the position along the edge is adjacent to one of the atoms then it will be reflected
--Checks if there are any additional atoms left to check, as if there are not we know the Marking wont be Absorb and wont be Reflect unless it is a Path that leads into that
    | (fst (atoms!!checkAtom) == snd edgePos-1 || fst (atoms!!checkAtom) == snd edgePos+1)
    && snd (atoms!!checkAtom) > snd currentPos
    && checkAtom+1 < length atoms
    = checkAbsorb n atoms edgePos checkAtom (snd edgePos, snd (atoms!!checkAtom)-1) (fst edgePos)
    
    | (fst (atoms!!checkAtom) == snd edgePos-1
    || fst (atoms!!checkAtom) == snd edgePos+1)
    && snd (atoms!!checkAtom) > snd currentPos
    = changeDirection n atoms edgePos checkAtom (snd edgePos, snd (atoms!!checkAtom)-1)

    | otherwise = checkNorth n atoms edgePos (checkAtom+1) currentPos

--Checks which atom a beam fired from East interacts with
checkEast :: Int -> Atoms -> EdgePos -> Int -> Pos -> Marking
checkEast n atoms edgePos checkAtom currentPos | checkAtom+1 > length atoms = Path (West, snd edgePos)
    | snd (atoms!!checkAtom) == snd edgePos
    && fst (atoms!!checkAtom) < fst currentPos = Absorb
    
    | (snd (atoms!!checkAtom) == snd edgePos-1 || snd (atoms!!checkAtom) == snd edgePos+1)
    && fst (atoms!!checkAtom) < fst currentPos
    && checkAtom+1 < length atoms
    = checkAbsorb n atoms edgePos checkAtom (fst (atoms!!checkAtom)+1, snd edgePos) (fst edgePos)
    
    | (snd (atoms!!checkAtom) == snd edgePos-1 || snd (atoms!!checkAtom) == snd edgePos+1) && fst (atoms!!checkAtom) < fst currentPos = changeDirection n atoms edgePos checkAtom (fst (atoms!!checkAtom)+1, snd edgePos)
    | otherwise = checkEast n atoms edgePos (checkAtom+1) currentPos

--Checks which atom a beam fired from South interacts with
checkSouth :: Int -> Atoms -> EdgePos -> Int -> Pos -> Marking
--Test checkSouth n atoms edgePos checkAtom currentPos | checkAtom+1 > length atoms = Absorb
checkSouth n atoms edgePos checkAtom currentPos | checkAtom+1 > length atoms = Path (North, snd edgePos)
    
    | fst (atoms!!checkAtom) == snd edgePos
    && snd (atoms!!checkAtom) < snd currentPos = Absorb
    
    | (fst (atoms!!checkAtom) == snd edgePos-1 || fst (atoms!!checkAtom) == snd edgePos+1)
    && snd (atoms!!checkAtom) < snd currentPos
    && checkAtom+1 < length atoms
    = checkAbsorb n atoms edgePos checkAtom (snd edgePos, snd (atoms!!checkAtom)+1) (fst edgePos)
    
    | (fst (atoms!!checkAtom) == snd edgePos-1 || fst (atoms!!checkAtom) == snd edgePos+1)
    && snd (atoms!!checkAtom) < snd currentPos
    = changeDirection n atoms edgePos checkAtom (snd edgePos, snd (atoms!!checkAtom)+1)
    
    | otherwise = checkSouth n atoms edgePos (checkAtom+1) currentPos

--Checks which atom a beam fired from West interacts with
checkWest :: Int -> Atoms -> EdgePos -> Int -> Pos -> Marking
checkWest n atoms edgePos checkAtom currentPos | checkAtom+1 > length atoms = Path (East, snd edgePos)
    
    | snd (atoms!!checkAtom) == snd edgePos && fst (atoms!!checkAtom) > fst currentPos = Absorb
    
    | (snd (atoms!!checkAtom) == snd edgePos-1 || snd (atoms!!checkAtom) == snd edgePos+1)
    && fst (atoms!!checkAtom) > fst currentPos
    && checkAtom+1 < length atoms
    = checkAbsorb n atoms edgePos checkAtom (fst (atoms!!checkAtom)-1, snd edgePos) (fst edgePos)
    
    | (snd (atoms!!checkAtom) == snd edgePos-1 || snd (atoms!!checkAtom) == snd edgePos+1)
    && fst (atoms!!checkAtom) > fst currentPos
    = changeDirection n atoms edgePos checkAtom (fst (atoms!!checkAtom)-1, snd edgePos)
    
    | otherwise = checkWest n atoms edgePos (checkAtom+1) currentPos


--Checks the case where two atoms are adjacent to ensure that absorbing takes priority over reflecting
checkAbsorb :: Int -> Atoms -> EdgePos -> Int -> Pos -> Side -> Marking
checkAbsorb n atoms edgePos checkAtom currentPos direction | checkAtom+1 > length atoms = changeDirection n atoms edgePos checkAtom currentPos
--Given that the beam is due to be reflected, we check if there is an atom directly infront of the beam that would absorb it as this has priority
    
    | (direction == North || direction == South)
    && atoms!!(checkAtom+1) == (fst currentPos, snd (atoms!!checkAtom))
    = Absorb
    
    | (direction == East || direction == West)
    && atoms!!(checkAtom+1) == (fst (atoms!!checkAtom), snd currentPos)
    = Absorb
    
    | otherwise = checkReflect n atoms edgePos checkAtom currentPos

--Checks the case atoms are reflected back in the direction they came from
checkReflect :: Int -> Atoms -> EdgePos -> Int -> Pos -> Marking
checkReflect n atoms edgePos checkAtom currentPos | fst currentPos == 0
    || fst currentPos > n
    || snd currentPos == 0
    || snd currentPos > n
    = Reflect --If either are zero or n+1 it's the first reflection and is on the first column/row
--Checking if there is another atom on the other side of the beam as it will be reflected if this is the case
    | (fst edgePos == North || fst edgePos == South)
    && atoms!!(checkAtom+1) == (fst currentPos +1, snd (atoms!!checkAtom))
    = Reflect

    | (fst edgePos == East || fst edgePos == West)
    && atoms!!(checkAtom+1) == (fst (atoms!!checkAtom), snd (atoms!!checkAtom)+1)
    = Reflect

    | otherwise = changeDirection n atoms edgePos checkAtom currentPos


--Determines which direction the beam is coming from after a reflection
changeDirection :: Int -> Atoms -> EdgePos -> Int -> Pos -> Marking
--The beam has been reflected so based on the direction it came from and the current positon of the beam in relation to the atom reflecting it we can determine the new direction
--The checkDirection function is then called in order to re-sort the atoms for this new direction before repeating all previous steps until the beam exits the box
changeDirection n atoms edgePos checkAtom currentPos | (fst edgePos == East || fst edgePos == West)
    && snd (atoms!!checkAtom) == snd currentPos +1
    = checkDirection n atoms (South, fst currentPos) currentPos
    
    | (fst edgePos == North || fst edgePos == South)
    && fst (atoms!!checkAtom) == fst currentPos -1
    = checkDirection n atoms (West, snd currentPos) currentPos
    
    | (fst edgePos == East || fst edgePos == West) && snd (atoms!!checkAtom)
    == snd currentPos -1 = checkDirection n atoms (North, fst currentPos) currentPos
    
    | otherwise = checkDirection n atoms (East, snd currentPos) currentPos

-- Challenge 2
-- Solve a Black Box

solveBB :: Int -> Interactions -> Atoms
solveBB x interactions = head $ filterBoards (generateBoards x (getDimensions interactions) (getAllAtoms (getDimensions interactions) 1 1)) interactions (getDimensions interactions) 0

--Returns the side length of the box
getDimensions :: Interactions -> Int
getDimensions [] = 0
getDimensions interactions = snd $ fst $ minimumBy (flip compare `on` fst) interactions

--init countCol, countRow = 1
--Creates a list of all atom positions
getAllAtoms :: Int -> Int -> Int -> Atoms
getAllAtoms n countCol countRow | countRow > n = []
    | countCol > n = getAllAtoms n 1 (countRow+1)
    | otherwise = (countCol, countRow) : getAllAtoms n (countCol+1) countRow

--Needs to be divided by x! before use, becomes n^2 C x
--init count = 0, n2 = n^2
makencrMult :: Int -> Int -> Int -> Int
makencrMult x n2 count | count < x = n2 * makencrMult x (n2-1) (count+1)
    | otherwise = 1

--generates all possible combinations that x atoms could have on a board
generateBoards :: Int -> Int -> Atoms -> [Atoms]
generateBoards x n allAtoms = createAllBoards x n allAtoms (replicate x 0) (makencrMult x (n^2) 0 `div` product [1..x])


--init count = 0, gaps = [0..0] length x
--first element of gaps corresponds to the distance between the first atom and zero, subsequent elements of gaps corresponds to distance from previous atom on board
--each time it iterates through creates a new board state
createAllBoards :: Int -> Int -> Atoms -> [Int] -> Int -> [Atoms]
createAllBoards x n allAtoms gaps ncr | ncr > 0 = createBoard x n allAtoms 0 gaps : createAllBoards x n allAtoms (updateGaps x n 0 (take (x-1) gaps ++ [gaps!!(x-1) +1])) (ncr-1)
    | otherwise = []

--init checking = 0
--Changes the values of the distances between atoms for different boards
updateGaps :: Int -> Int -> Int -> [Int] -> [Int]
updateGaps x n checking gaps | checking > x-1 = gaps
    | (sum (take (checking+1) gaps) + checking) < n^2 = updateGaps x n (checking+1) gaps
    | otherwise = updateGaps x n 0 (take (checking-1) gaps ++ (gaps!!(checking-1) +1) : replicate (x-checking) 0) --runs again to check if the change made the previous value go outside of the range of atoms

--Creates a list of x coordinates
createBoard :: Int -> Int -> Atoms -> Int -> [Int] -> Atoms
createBoard x n allAtoms count gaps | count < x = allAtoms!!(count + sum (take (count+1) gaps)) : createBoard x n allAtoms (count+1) gaps
    | otherwise = []

--Repeatedly fires beams at each board state until runs out of positions to fire beams from
filterBoards :: [Atoms] -> Interactions -> Int -> Int -> [Atoms]
filterBoards [] _ _ _ = [[]]
filterBoards atoms interactions n count | count+2 > length interactions = atoms
    | otherwise = filterBoards (filterOneBeam atoms interactions n count) interactions n (count+1)


--Fires a beam at all board states
filterOneBeam :: [Atoms] -> Interactions -> Int -> Int -> [Atoms]
filterOneBeam [] _ _ _ = []
filterOneBeam atoms interactions n count | fireBeam n (head atoms) (fst $ interactions!!count) == interactions!!count = head atoms : filterOneBeam (tail atoms) interactions n count
    | otherwise = filterOneBeam (tail atoms) interactions n count

-- Challenge 3
-- Pretty Printing Lambda with Scott Numerals

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int
                deriving (Eq, Show, Read)


prettyPrint :: LamExpr -> String
prettyPrint (LamApp (LamApp a b) (LamApp c d)) = prettyPrint a ++ " " ++ prettyPrint b ++ " (" ++ prettyPrint c ++ " " ++ prettyPrint d ++ ")"
prettyPrint (LamApp (LamApp a b) c) = prettyPrint a ++ " " ++ prettyPrint b ++ " " ++ prettyPrint c
prettyPrint (LamApp (LamAbs a b) (LamApp c d)) = "(\\x" ++ show a ++ " -> " ++ prettyPrint b ++ ") (" ++ prettyPrint c ++ " " ++ prettyPrint d ++ ")"
prettyPrint (LamApp a (LamApp b c)) = prettyPrint a ++ " (" ++ prettyPrint b ++ " " ++ prettyPrint c ++ ")"
prettyPrint (LamApp (LamAbs a b) c) = "(\\x" ++ show a ++ " -> " ++ prettyPrint b ++ ") " ++ prettyPrint c
prettyPrint (LamApp a b) = prettyPrint a ++ " " ++ prettyPrint b

prettyPrint (LamAbs a (LamAbs b c)) = show (scottEnc (LamAbs b c))
prettyPrint (LamAbs a (LamApp b c)) = "\\x" ++ show a ++ " -> " ++ prettyPrint (LamApp b c)
prettyPrint (LamAbs a b) = "\\x" ++ show a ++ " -> " ++ prettyPrint b

prettyPrint (LamVar a) = "x" ++ show a

--When there is consecutive abstraction, is run in order to simplify
scottEnc :: LamExpr -> Int
scottEnc (LamAbs a (LamVar b)) = 0
scottEnc (LamAbs a (LamApp b (LamAbs c d))) = 1 + scottEnc (LamAbs c d) --Runs recursively for a continuing cycle of application and abstraction
scottEnc (LamAbs a (LamApp b c)) = 1
scottEnc (LamAbs a (LamAbs b c)) = 0

-- Challenge 4 
-- Parsing Let Expressions

data LetExpr =  LetApp LetExpr LetExpr | LetDef [([Int], LetExpr)] LetExpr |
                LetFun Int | LetVar Int | LetNum Int
                deriving (Show, Eq)


--the base function which is iterated by lapp
lnum :: Parser LetExpr
lnum = do v <- lvar
          return v
        <|> do n <- digit
               return (LetNum (charToNum n))

--converts a character of a digit to an integer, used in functions after parsing digit as it returns a character
charToNum :: Char -> Int
charToNum '0' = 0
charToNum '1' = 1
charToNum '2' = 2
charToNum '3' = 3
charToNum '4' = 4
charToNum '5' = 5
charToNum '6' = 6
charToNum '7' = 7
charToNum '8' = 8
charToNum '9' = 9

--checks letvar goes to lfun
lvar :: Parser LetExpr
lvar = do f <- lfun
          return f
        <|> do symbol "x"
               n <- digit
               return (LetVar (charToNum n))

--checks letfun goes to ldef
lfun :: Parser LetExpr
lfun = do d <- ldef
          return d
        <|> do symbol "f"
               n <- digit
               return (LetFun (charToNum n))

--checks letdef goes to brackets
ldef :: Parser LetExpr
ldef = do b <- brackets
          return b
        <|> do symbol "let"
               stuffs <- partldef
               symbol "in"
               a <- lapp
               return (LetDef stuffs a)

--used by ldef to check for multiple equations in an EqnList
partldef :: Parser [([Int], LetExpr)]
partldef = do f <- lfun
              v0 <- lvar
              symbol "="
              a <- lapp
              do symbol ";"
                 cont <- partldef
                 return (([conversion f, conversion v0], a) : cont)
                <|> return [([conversion f, conversion v0], a)]
            <|> return []


--converts a datatype to its integer part
conversion :: LetExpr -> Int
conversion (LetFun n) = n
conversion (LetVar n) = n
conversion (LetNum n) = n

--the base parsing function that is called by parselet
lapp :: Parser LetExpr
lapp = do e0 <- lnum
          do e1 <- brackets --ensures that letApp doesnt change how brackets will associate
             return (LetApp e0 e1)
            <|> do e2 <- lapp
                   return $ reorderLapp (LetApp (e0) (e2)) --changes letApp to associate to the left
                <|> return e0

--reorders the letApp
reorderLapp :: LetExpr -> LetExpr
reorderLapp (LetApp a (LetApp b c)) = LetApp (LetApp a b) c
reorderLapp other = other

--the final check, where it terminates
brackets :: Parser LetExpr
brackets = do symbol "("
              e <- lapp
              symbol ")"
              return e


parseLet :: String -> Maybe LetExpr
parseLet str = case (parse lapp str) of
                  [(n, [])] -> Just n
                  [(_, out)] -> Nothing
                  [] -> Nothing


-- Challenge 5
-- Encode lambda terms as combinators 

data CLExpr = S | K  | I | CLVar Int | CLApp CLExpr CLExpr
              deriving (Show,Read,Eq)



clTransform :: LamExpr -> CLExpr
clTransform (LamApp a b) = CLApp (clTransform a) (clTransform b)
clTransform (LamAbs a b) = convert (clTransform b) a
clTransform (LamVar a) = CLVar a


convert :: CLExpr -> Int -> CLExpr
convert (CLApp expr0 expr1) x | not $ checkInside (CLApp expr0 expr1) = CLApp (CLApp S (convert expr0 x)) (convert expr1 x) -- \\x -> e1 e2 == S CLApp (\\x -> e1 \\x -> e2)
    | otherwise = CLApp K (CLApp expr0 expr1) --If inside of CLApp already fully converted LamAbs x expr == CLApp K expr
convert (CLVar n) x | n == x = I -- \\x -> x == I
    | otherwise = CLApp K (CLVar n) -- \\x -> y == K y
convert expr _ = CLApp K expr

--checks if the interior of CLApp is fully converted or not
checkInside :: CLExpr -> Bool
checkInside (CLApp (CLApp a b) (CLApp c d)) = checkInside (CLApp a b) && checkInside (CLApp c d)
checkInside (CLApp (CLApp a b) I) = checkInside (CLApp a b)
checkInside (CLApp S (CLApp a b)) = checkInside (CLApp a b)
checkInside (CLApp K (CLApp a b)) = checkInside (CLApp a b)
checkInside (CLApp I (CLApp a b)) = checkInside (CLApp a b)
checkInside (CLApp S I) = True
checkInside (CLApp S K) = True
checkInside (CLApp K K) = True
checkInside (CLApp K I) = True
checkInside (CLApp I K) = True
checkInside (CLApp I I) = True
checkInside _ = False --Impossible for CLApp to end with S, therefore not checked


-- Challenge 6
-- Compare Innermost and Outermost Reduction for Lambda and Combinators 




data TLamExpr = TLamVar String | TLamAbs String TLamExpr | TLamApp TLamExpr TLamExpr deriving (Eq, Show, Read)

--modified material from lectures
free :: String -> TLamExpr -> Bool
free a (TLamVar b) = a == b
free a (TLamAbs b expr) | a == b = False
    | otherwise = free a expr
free a (TLamApp expr0 expr1) = free a expr0 || free a expr1

--modified material from lectures
subst :: TLamExpr -> String -> TLamExpr -> TLamExpr
subst (TLamVar a) b expr | a == b = expr
    | otherwise = TLamVar a
subst (TLamAbs a expr0) b expr1 | a /= b && not (free a expr1) = TLamAbs a (subst expr0 b expr1)
    | a /= b = let a' = rename a expr0 in subst (TLamAbs a' (subst expr0 a (TLamVar a'))) b expr1
    | otherwise = TLamAbs a expr0
subst (TLamApp expr0 expr1) a expr2 = TLamApp (subst expr0 a expr2) (subst expr1 a expr2)

--modified material from lectures
rename :: [Char] -> TLamExpr -> [Char]
rename a expr | free (a ++ "\'") expr = rename (a ++ "\'") expr
    | otherwise = a ++ "\'"

--converts from LamExpr to temporary type TLamExpr, used so that variables can be renamed
lamTemp :: LamExpr -> TLamExpr
lamTemp (LamVar a) = TLamVar (show a)
lamTemp (LamAbs a b) = TLamAbs (show a) (lamTemp b)
lamTemp (LamApp a b) = TLamApp (lamTemp a) (lamTemp b)

--converts from temporary type TLamExpr to LamExpr
tempLam :: TLamExpr -> LamExpr
tempLam (TLamVar a) = LamVar (read a)
tempLam (TLamAbs a b) = LamAbs (read a) (tempLam b)
tempLam (TLamApp a b) = LamApp (tempLam a) (tempLam b)

--performs outermost reduction on expression
--modified material from lectures
evalOut :: LamExpr -> LamExpr
evalOut (LamApp (LamAbs a expr0) expr1) = tempLam $ subst (lamTemp expr0) (show a) (lamTemp expr1)
evalOut (LamApp expr0 expr1) = LamApp (evalOut expr0) expr1
evalOut other = other



--performs innermost reduction on expression
evalIn :: LamExpr -> LamExpr
evalIn (LamApp (LamApp expr0 expr1) (LamApp expr2 expr3)) | countNest (LamApp expr0 expr1) < countNest (LamApp expr2 expr3) = LamApp (LamApp expr0 expr1) (evalIn $ LamApp expr2 expr3) --checks if there are nested expressions within the expression and which part of LamApp has more nested statements
    | otherwise = LamApp (evalIn $ LamApp expr0 expr1) (LamApp expr2 expr3)
evalIn (LamApp (LamAbs a expr0) expr1) | countNest expr0 < countNest expr1 = LamApp (LamAbs a expr0) (evalIn expr1) --checks for nested expressions in LamAbs to determine which side should be checked first in an innermost reduction
    | otherwise = tempLam $ subst (lamTemp expr0) (show a) (lamTemp expr1)
evalIn (LamApp expr0 expr1) | countNest expr0 < countNest expr1 = LamApp expr0 (evalIn expr1) --checks for nested expressions in any other case (e.g. when only the second expression is LamApp)
    | otherwise = LamApp (evalIn expr0) expr1
evalIn other = other --if the expression is fully reduced

--used to check how many nested expressions (LamApp) are within an expression
countNest :: LamExpr -> Int
countNest (LamApp expr0 expr1) | countNest expr0 < countNest expr1 = 1 + countNest expr1
    | otherwise = 1 + countNest expr0
countNest (LamAbs a (LamApp expr0 expr1)) | countNest expr0 < countNest expr1 = countNest expr1
    | otherwise = countNest expr0
countNest _ = 0




--performs outermost reduction on CL expression
evalCLOut :: CLExpr -> CLExpr
evalCLOut (CLApp (CLApp K expr0) expr1) = expr0 -- K x y == x
evalCLOut (CLApp I expr0) = expr0 -- I x = x
evalCLOut (CLApp K (CLApp expr0 expr1)) = expr0 -- K x y == x
evalCLOut (CLApp S (CLApp (CLApp expr0 expr1) expr2)) = CLApp (CLApp expr0 expr2) (CLApp expr1 expr2) --S x y z == x z (y z)
evalCLOut (CLApp S (CLApp expr0 (CLApp expr1 expr2))) = CLApp (CLApp expr0 expr2) (CLApp expr1 expr2) --S x y z == x z (y z)
evalCLOut (CLApp S (CLApp K K)) = I --S K K == I
evalCLOut (CLApp expr0 expr1) | expr0 == evalCLOut expr0 = CLApp expr0 (evalCLOut expr1) --if no reduction has been performed looks inside
    | otherwise = CLApp (evalCLOut expr0) expr1
evalCLOut other = other --If no reductions left to be made




--performs innermost reduction on CL expression
evalCLIn :: CLExpr -> CLExpr
evalCLIn (CLApp (CLApp expr0 expr1) (CLApp expr2 expr3)) | CLApp expr0 expr1 == evalCLIn (CLApp expr0 expr1) = CLApp (CLApp expr0 expr1) (evalCLIn $ CLApp expr2 expr3) -- looks if there are nested expressions
    | otherwise = CLApp (evalCLIn $ CLApp expr0 expr1) (CLApp expr2 expr3)
evalCLIn (CLApp expr0 expr1) | countCLNest expr0 > 1 = CLApp (evalCLIn expr0) expr1 --looks for nested expressions
    | countCLNest expr1 > 1 = CLApp expr0 (evalCLIn expr1)
evalCLIn (CLApp (CLApp K expr0) expr1) = expr0 --K x y == x
evalCLIn (CLApp I expr0) = expr0 --I x == x
evalCLIn (CLApp K (CLApp expr0 expr1)) = expr0 -- K x y == x
evalCLIn (CLApp S (CLApp (CLApp expr0 expr1) expr2)) = CLApp (CLApp expr0 expr2) (CLApp expr1 expr2) --S x y z == x z (y z)
evalCLIn (CLApp S (CLApp expr0 (CLApp expr1 expr2))) = CLApp (CLApp expr0 expr2) (CLApp expr1 expr2) --S x y z == x z (y z)
evalCLIn (CLApp S (CLApp K K)) = I --S K K == I
evalCLIn other = other --If no reductions left to be made

--used to check how many nested expressions (CLApp) are within an expression
countCLNest :: CLExpr -> Int
countCLNest (CLApp expr0 expr1) | countCLNest expr0 < countCLNest expr1 = 1 + countCLNest expr1
    | otherwise = 1 + countCLNest expr0
countCLNest _ = 0




--calls evalOut recursively up to n times or until it terminates
runOut :: LamExpr -> Int -> Int -> Maybe Int
runOut (LamApp expr0 expr1) n count | LamApp expr0 expr1 == evalOut (LamApp expr0 expr1) = Nothing --checks if evalOut has terminated but is still not in normal form
    | n < count+1 = Nothing --checks if has the reduction has hit the max number of steps
    | otherwise = runOut (evalOut (LamApp expr0 expr1)) n (count+1) --if not runs again
runOut _ _ count = Just count --if is in normal form returns the count

--calls evalIn recursively up to n times or until it terminates
runIn :: LamExpr -> Int -> Int -> Maybe Int
runIn (LamApp expr0 expr1) n count | LamApp expr0 expr1 == evalIn (LamApp expr0 expr1) = Nothing --checks if evalOut has terminated but is still not in normal form
    | n < count+1 = Nothing --checks if has the reduction has hit the max number of steps
    | otherwise = runIn (evalIn (LamApp expr0 expr1)) n (count+1) --if not runs again
runIn _ _ count = Just count --if is in normal form returns the count

--calls evalCLOut recursively up to n times or until it terminates
runCLOut :: CLExpr -> Int -> Int -> Maybe Int
--checks if the sequence has terminated and if it is in normal form or not
runCLOut (CLApp (CLApp (CLApp expr0 expr1) expr2) expr3) n count | CLApp (CLApp (CLApp expr0 expr1) expr2) expr3 == evalCLOut (CLApp (CLApp (CLApp expr0 expr1) expr2) expr3) = Nothing
    | otherwise = runningCLOut (CLApp (CLApp (CLApp expr0 expr1) expr2) expr3) n (count+1)
runCLOut (CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3) n count | CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3 == evalCLOut (CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3) = Nothing
    | otherwise = runningCLOut (CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3) n (count+1)
runCLOut (CLApp expr0 (CLApp (CLApp expr1 expr2) expr3)) n count | CLApp expr0 (CLApp (CLApp expr1 expr2) expr3) == evalCLOut (CLApp expr0 (CLApp (CLApp expr1 expr2) expr3)) = Nothing
    | otherwise = runningCLOut (CLApp expr0 (CLApp (CLApp expr1 expr2) expr3)) n (count+1)
runCLOut (CLApp expr0 (CLApp expr1 (CLApp expr2 expr3))) n count | CLApp expr0 (CLApp expr1 (CLApp expr2 expr3)) == evalCLOut (CLApp expr0 (CLApp expr1 (CLApp expr2 expr3))) = Nothing
    | otherwise = runningCLOut (CLApp expr0 (CLApp expr1 (CLApp expr2 expr3))) n (count+1)
runCLOut expr n count | evalCLOut expr == expr = Just count
    | otherwise = runningCLOut expr n (count+1)

--saves a few lines, just checks if n has been reached, if not calls runCLOut again
runningCLOut :: CLExpr -> Int -> Int -> Maybe Int
runningCLOut expr n count | n < count = Nothing
    | otherwise = runCLOut (evalCLOut expr) n count

-- calls evalCLIn recursively up to n times or until it terminates
runCLIn :: CLExpr -> Int -> Int -> Maybe Int
--checks if the sequence has terminated and if it is in normal form or not
runCLIn (CLApp (CLApp (CLApp expr0 expr1) expr2) expr3) n count | CLApp (CLApp (CLApp expr0 expr1) expr2) expr3 == evalCLIn (CLApp (CLApp (CLApp expr0 expr1) expr2) expr3) = Nothing
    | otherwise = runningCLIn (CLApp (CLApp (CLApp expr0 expr1) expr2) expr3) n (count+1)
runCLIn (CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3) n count | CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3 == evalCLIn (CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3) = Nothing
    | otherwise = runningCLIn (CLApp (CLApp expr0 (CLApp expr1 expr2)) expr3) n (count+1)
runCLIn (CLApp expr0 (CLApp (CLApp expr1 expr2) expr3)) n count | CLApp expr0 (CLApp (CLApp expr1 expr2) expr3) == evalCLIn (CLApp expr0 (CLApp (CLApp expr1 expr2) expr3)) = Nothing
    | otherwise = runningCLIn (CLApp expr0 (CLApp (CLApp expr1 expr2) expr3)) n (count+1)
runCLIn (CLApp expr0 (CLApp expr1 (CLApp expr2 expr3))) n count | CLApp expr0 (CLApp expr1 (CLApp expr2 expr3)) == evalCLIn (CLApp expr0 (CLApp expr1 (CLApp expr2 expr3))) = Nothing
    | otherwise = runningCLIn (CLApp expr0 (CLApp expr1 (CLApp expr2 expr3))) n (count+1)
runCLIn expr n count | evalCLIn expr == expr = Just count
    | otherwise = runningCLIn expr n (count+1)

--saves a few lines, just checks if n has been reached, if not calls runCLIn again
runningCLIn :: CLExpr -> Int -> Int -> Maybe Int
runningCLIn expr n count | n < count = Nothing
    | otherwise = runCLIn (evalCLIn expr) n count


innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 expr | evalIn expr == expr = Nothing
    | otherwise = Just $ evalIn expr


outerRedn1 :: LamExpr -> Maybe LamExpr
outerRedn1 expr | evalOut expr == expr = Nothing
    | otherwise = Just $ evalOut expr


innerCLRedn1 :: CLExpr -> Maybe CLExpr
innerCLRedn1 expr | evalCLIn expr == expr = Nothing
    | otherwise = Just $ evalCLIn expr


outerCLRedn1 :: CLExpr -> Maybe CLExpr
outerCLRedn1 expr | evalCLOut expr == expr = Nothing
    | otherwise = Just $ evalCLOut expr


compareInnerOuter :: LamExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter expr n = (runIn expr n 0, runOut expr n 0, runCLIn (clTransform expr) n 0, runCLOut (clTransform expr) n 0)
