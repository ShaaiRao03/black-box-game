import Data.List
import Text.Read (readMaybe)


type CurPos = (Side,Pos)  -- Current Position 
type Pos = (Int,Int)
type EdgePos = (Side, Int)
type Atoms = [Pos]
type Interactions = [(EdgePos , Marking)]
data Side = North | East | South | West deriving (Show,Eq,Ord,Read)
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq,Read)


-- ---------------- Challenge 1 ---------------------------  

calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions gridNum atomsPos = generateInteraction atomsPos gridNum (rays gridNum) []

rays :: (Num b, Enum b) => b -> [(Side, b)] 
rays gridNum = [ (col,row) | col <- [North,East,South,West]  , row <- [1..gridNum] ]

generateInteraction :: Atoms -> Int  -> [EdgePos] -> Interactions -> Interactions
generateInteraction _ _ [] x = x
generateInteraction atomsPos gridNum (x:xs) interactions =  generateInteraction atomsPos gridNum xs (pathFinder atomsPos x curPos dftPos True : interactions)
    where
        dftPos = generateDftPos atomsPos [] 
        curPos = getCurPos gridNum x

-- get current position
getCurPos :: Int -> EdgePos -> (Side, Pos)
getCurPos gridNum (dir, val)
    | dir == North = (dir, (val, 1))
    | dir == South = (dir, (val, gridNum))
    | dir == East  = (dir, (gridNum, val))
    | otherwise = (dir, (1, val))  


-- to generate interactions 
pathFinder :: Atoms -> EdgePos -> CurPos -> [Pos] -> Bool -> (EdgePos , Marking)
pathFinder atomsPos startPos curPos dftPos firstIteration
    | firstIteration && pos `elem` generateEdgeReflectionPos = (startPos, Reflect) -- for edge reflection
    | not (null (sameDirPos curPos atomsPos)) && checkAbsorbtion curPos atomsPos (sameDirPos curPos dftPos) = (startPos,Absorb) -- for absorbtion 
    | not (isDftPosExist curPos dftPos) = if startPos == finalStop curPos then (startPos,Reflect) else (startPos, Path (finalStop curPos) ) -- for reflect / path
    | otherwise =  pathFinder atomsPos startPos (nextStop atomsPos curPos dftPos) dftPos False
    where
        (dir,pos) = curPos
        val = if dir `elem` [North,South] then fst pos else snd pos
        getPotentialEdgeReflectionAtoms = potentialEdgeReflectionAtoms curPos atomsPos
        generateEdgeReflectionPos =
             if dir `elem` [North, South]
                then [ (col+1, row)  | (col,row) <- getPotentialEdgeReflectionAtoms] ++ [ (col-1, row)  | (col,row) <- getPotentialEdgeReflectionAtoms]
                else [ (col, row+1)  | (col,row) <- getPotentialEdgeReflectionAtoms] ++ [ (col, row-1)  | (col,row) <- getPotentialEdgeReflectionAtoms]  


checkAbsorbtion :: CurPos -> [Pos] -> [Pos] -> Bool
checkAbsorbtion (dir,(col,row)) atomsPos dftPos
    | (col,row) `elem` atomsPos = True -- if the position of curPos is the same as the atomPos  
    | not (isElemBetweenPts (col,row) closestAtom dftPos)  = True  -- if there's an atom on the same path as the curPos and there's no deflection points btween them
    | specialCaseAbsorbtion (dir,(col,row)) atomsPos dftPos = True -- side by side atoms
    | otherwise = False
    where
        sameDirAtoms = sameDirPos (dir,(col,row)) atomsPos -- atoms that are on the same path as curPos
        closestAtom = if length sameDirAtoms > 1 then closestPoint (col,row) sameDirAtoms else head sameDirAtoms 

-- side by side
specialCaseAbsorbtion :: CurPos ->  [Pos]  ->  [Pos] -> Bool
specialCaseAbsorbtion (dir,(col,row)) atomsPos dftPos
    | dir == North && (col,row) `elem` dftPos && (all (`elem` atomsPos) [(col+1,row+1),(col,row+1)] || all (`elem` atomsPos) [(col-1,row+1),(col,row+1)]) = True
    | dir == South && (col,row) `elem` dftPos && (all (`elem` atomsPos) [(col+1,row-1),(col,row-1)] || all (`elem` atomsPos) [(col-1,row-1),(col,row-1)]) = True
    | dir == West && (col,row) `elem` dftPos && (all (`elem` atomsPos) [(col+1,row+1),(col+1,row)]  || all (`elem` atomsPos) [(col+1,row-1),(col+1,row)]) = True
    | dir == East && (col,row) `elem` dftPos && (all (`elem` atomsPos) [(col-1,row-1),(col-1,row)] || all (`elem` atomsPos) [(col-1,row+1),(col-1,row)]) = True
    | otherwise = False

-- check if there's any other points within given two points 
isElemBetweenPts :: Pos -> Pos -> [Pos] -> Bool
isElemBetweenPts (curCol, curRow) (atomCol, atomRow) pts =
    any (\(col, row) -> col == curCol && (row >= curRow && row < atomRow || row > atomRow && row <= curRow)) pts
    || any (\(col, row) -> row == curRow && (col <= curCol && col > atomCol ||  col >= curCol && col < atomCol)) pts


-- to obtain the final stop for the coordinate  
finalStop :: CurPos -> EdgePos
finalStop (dir,(col1,row1))
    | dir == North = (South,col1)
    | dir == South = (North,col1)
    | dir == East = (West,row1)
    | otherwise = (East,row1)

-- to get atoms given a deflection point
getAtom :: Atoms -> Pos -> Pos
getAtom atomsPos (col,row)
    | (col-1,row+1) `elem` atomsPos = (col-1,row+1) -- top right 
    | (col-1,row-1) `elem` atomsPos = (col-1,row-1) -- bottom right
    | (col+1,row-1) `elem` atomsPos = (col+1,row-1) -- bottom left
    | otherwise = (col+1,row+1) -- top left

-- determines the next stop for the curPos  
nextStop :: Atoms -> CurPos -> [Pos] -> CurPos
nextStop atomsPos curPos dftPos
    | snd curPos `elem` dftPos  = deflectionLogic (getAtom atomsPos (snd curPos)) curPos dftPos  --check if its on deflection point , if yes , do deflection logic , return curPos which has been deflected
    | isDftPosExist curPos dftPos = if length potentialDftPos > 1 then (dir,closestPoint position potentialDftPos) else (dir,head potentialDftPos) --if not on deflection point , find nearest deflection point and move curPos to the deflection point
    | otherwise = curPos -- return the curPos (which means there's no deflection points onwards / the point is not on the deflection point)
    where
        potentialDftPos = sameDirPos curPos dftPos
        (dir,position) =  curPos

-- contains all the deflection logic  
deflectionLogic :: Pos -> CurPos -> [Pos] -> CurPos
deflectionLogic atomPos curPos dftPos
    | countOccurrences dftPos (snd curPos) > 1 = (getOpposite curPos)                -- for double reflection (if there's more then one deflection point at the same position, then change take the opposite dir)
    | positionCoord == (col-1,row-1)  && dir  == North = (East,(curCol-1,curRow))    -- top_left
    | positionCoord == (col-1,row-1)  && dir  == West  = (South,(curCol,curRow-1))   -- top_left
    | positionCoord == (col+1,row-1) && dir   == North = (West,(curCol+1,curRow))    -- top_right
    | positionCoord == (col+1,row-1) && dir   == East  = (South,(curCol,curRow-1))   -- top_right
    | positionCoord == (col-1,row+1) && dir   == West  = (North,(curCol,curRow+1))   -- bottom_left
    | positionCoord == (col-1,row+1) && dir   == South = (East,(curCol-1,curRow))    -- bottom_left
    | positionCoord == (col+1,row+1) && dir   == South = (West,(curCol+1,curRow))    -- bottom_right
    | otherwise = (North,(curCol,curRow+1))  -- bottom_right 
    where
        (dir,positionCoord) = curPos
        (curCol,curRow) = positionCoord
        (col,row) = atomPos

-- for double reflection (if there's two deflection point on the same position but its not special case absorbtion ) 
getOpposite :: CurPos -> CurPos
getOpposite (dir,(col1,row1))
    | dir == North = (South,(col1,row1-1))
    | dir == South = (North,(col1,row1+1))
    | dir == West  = (East,(col1-1,row1))
    | otherwise = (West,(col1+1,row1))

-- generates all the deflection positions from all 4 sides for each atoms
generateDftPos :: [Pos] -> [Pos] -> [Pos]
generateDftPos [] z = z
generateDftPos (x:xs) deflectionSet =
    generateDftPos xs (((fst x)-1,(snd x)-1) : ((fst x)+1,(snd x)-1) : ((fst x)-1,(snd x)+1) : ((fst x)+1,(snd x)+1) : deflectionSet)

-- get all positions within the same column/row  
sameDirPos :: CurPos -> [Pos] -> [Pos]
sameDirPos (dir,(col1,row1)) pts 
    | dir == North = filter (\(col, row) -> row1 <= row && col1 == col ) pts
    | dir == South = filter (\(col, row) -> row1 >= row && col1 == col ) pts
    | dir == East = filter (\(col, row) -> row1 == row && col1 >= col ) pts
    | otherwise =  filter (\(col, row) -> row1 == row && col1 <= col) pts

-- check if a coordinate is on deflection position 
isDftPosExist :: CurPos -> [Pos] -> Bool
isDftPosExist curPos dftPos
    | null (sameDirPos curPos dftPos) = False
    | otherwise = True

-- used to find edge reflection 
potentialEdgeReflectionAtoms :: CurPos -> [Pos] -> [Pos]
potentialEdgeReflectionAtoms (dir,(col1,row1)) atomPos
    | dir `elem` [North, South] =  filter (\(_, row) -> row == row1) atomPos
    | otherwise = filter (\(col, _) -> col == col1) atomPos

countOccurrences :: Eq a => [a] -> a -> Int
countOccurrences lst element = length $ filter (== element) lst

closestPoint :: Pos -> [Pos] -> Pos
closestPoint startPt pts =
    foldl1' (\acc cur -> if distance startPt cur < distance startPt acc then cur else acc) pts

distance :: Pos -> Pos -> Int 
distance (x1, y1) (x2, y2) = round (sqrt ((fromIntegral x2 - fromIntegral x1)^2 + (fromIntegral y2 - fromIntegral y1)^2)) 

-- ---------------- Challenge 2 --------------------------- 

solveBB :: Int -> Interactions -> [Atoms]
solveBB numAtoms interactions = generatePossibleAtomComb gridNum atomsCombination interactions []
    where
        gridNum =  getLargestValue interactions [] -- estimating the size of grid
        ls = getInvalidAtomPos gridNum interactions [] -- obtaining invalid positions of  
        atomsCombination = atomCombination numAtoms (filter (`notElem` ls) (generateGrid gridNum)) -- generates all possible atoms combination

-- to filter all invalid positions for atom before creating the combination 
getInvalidAtomPos :: Int -> Interactions -> [Pos] -> [Pos] 
getInvalidAtomPos _ [] z = z
getInvalidAtomPos gridNum (x:xs) z
    | snd x /= Absorb = getInvalidAtomPos gridNum xs (invalidAtomPos : z) 
    | otherwise = getInvalidAtomPos gridNum xs z
    where
        invalidAtomPos = snd (getCurPos gridNum (fst x)) 

generatePossibleAtomComb ::  Int -> [Atoms] -> Interactions -> [Atoms] -> [Atoms]
generatePossibleAtomComb _ [] _ output = output
generatePossibleAtomComb gridNum (x:xs) interactions output
    | isValidAtomComb gridNum x dftPos interactions = generatePossibleAtomComb gridNum xs interactions (x : output)
    | otherwise =  generatePossibleAtomComb gridNum xs interactions output
    where 
        dftPos = generateDftPos x []

-- check if the given list of atoms position satisfies the interactions
isValidAtomComb :: Int -> Atoms -> [Pos] -> Interactions -> Bool
isValidAtomComb _ _ _ [] = True
isValidAtomComb gridNum atomsPos dftPos (x:xs)
    | isInteractionValid atomsPos x curPos dftPos = isValidAtomComb gridNum atomsPos dftPos xs
    | otherwise = False
    where
        curPos = getCurPos gridNum (fst x)

-- to check if the interaction is matching or not
isInteractionValid :: Atoms -> (EdgePos , Marking) -> CurPos -> [Pos] -> Bool
isInteractionValid atomsPos expectedInteraction curPos dftPos
    | expectedInteraction == generatedInteraction = True
    | otherwise = False
    where 
        generatedInteraction = pathFinder atomsPos (fst expectedInteraction) curPos dftPos True

-- to generate grid so that we can use them to generate subsets based on N atoms specified by user 
generateGrid gridNum = [ (col,row) | col <- [1..gridNum]  , row <- [1..gridNum] ]

-- Source for code 'atomCombination' : https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
-- to generate all possible combinations of atoms position given the number of atoms 
atomCombination 0 _ = [[]]
atomCombination _ [] = []
atomCombination n (x : xs) = map (x :) (atomCombination (n - 1) xs) ++ atomCombination n xs

-- this is to obtain the largest value in the given interaction , by using the value we can estimate the size of the grid 
getLargestValue :: Interactions -> [Int] -> Int
getLargestValue [] z = maximum z
getLargestValue (x:xs) values = getLargestValue xs (obtainAllVal x ++ values)

-- obtaining all the numerical values within the interactions
obtainAllVal :: (EdgePos, Marking) -> [Int]
obtainAllVal (edgePos, marking) = do
  case marking of
    Absorb -> [val]
    Reflect -> [val]
    Path (_,val2) -> [val, val2]
  where 
    val = case edgePos of
      (_, v) -> v

-- ------ Read IO ----------------- 

--  to read an integer from user input 
readInt :: [Char] -> IO Int
readInt sentence = do
  putStrLn sentence 
  input <- getLine 
  case readMaybe input of
    Just n  -> return n
    Nothing -> putStrLn "Invalid input. Please enter a valid integer." >> readInt sentence  

--  to read a list of positions from user input 
readPositions :: IO [(Int, Int)]
readPositions = do
  putStrLn "Enter positions of atoms in the form [(Int, Int)]:"
  input <- getLine
  case readMaybe input of
    Just positions -> return positions
    Nothing -> putStrLn "Invalid input. Please enter a valid list of positions." >> readPositions

-- to read interactions from the user 
readInteractions :: IO [(EdgePos , Marking)]
readInteractions = do 
  putStrLn "Enter the interactions in the form [(EdgePos , Marking)]:"
  input <- getLine
  case readMaybe input of 
    Just positions -> return positions       
    Nothing -> putStrLn "Invalid input. Please enter a valid list of interactions." >> readInteractions


main :: IO()  
main = do

-- Challenge 1
    grid <- readInt "Enter a grid number (Integer) : "
    atomPositions <- readPositions
    let generatedInteractions = calcBBInteractions grid atomPositions
    putStrLn $ "\nGrid Number: " ++ show grid  
    putStrLn $ "Atom Positions: " ++ show atomPositions 
    putStrLn $ "Generated Interactions: " ++ show generatedInteractions ++ "\n"


-- Challenge 2
    numAtoms <- readInt "Enter the number of atoms (Integer) : "
    listInteractions <- readInteractions 
    let generatedAtomCombination = solveBB numAtoms listInteractions 
    putStrLn $ "\nNumber of atoms: " ++ show numAtoms  
    putStrLn $ "Given Interactions: " ++ show listInteractions 
    putStrLn $ "Possible position of atoms: " ++ show generatedAtomCombination  
    



