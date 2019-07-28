{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad.IO.Class 
import Data.List (reverse, transpose)
import Data.Vector (Vector)
import Data.Word (Word8)
import SDL (Renderer, ($=))
import SDL.Video.Renderer (fillRect, drawRect, rendererDrawColor, clear, present, Rectangle(..))
import SDL.Vect (V4(..), V2(..), Point(..))

import Foreign.C.Types (CInt(..))
import GHC.Int (Int32)
import GHC.Word (Word32)

import Control.Lens hiding (element)
import Control.Lens.TH


data Colour = Red | Green | Blue | Yellow | Pink | Purple | Orange | White deriving Show

newtype BrickShape = BrickShape {_unBrickShape :: [[Bool]]}  deriving Show

data Brick = Brick { 
    _shape :: BrickShape, 
    _colour :: Colour 
} deriving Show

data ActiveBrick = ActiveBrick {
    _brick :: Brick,
    _x :: Int,
    _y :: Int
} deriving Show

data State = State {
    _active :: ActiveBrick,
    _nextShapes :: [BrickShape],
    _nextColours :: [Colour],
    _currentGrid :: [[Maybe Colour]],
    _lastTick :: Word32
} deriving Show

$(makeLenses ''BrickShape)
$(makeLenses ''Brick)
$(makeLenses ''ActiveBrick)
$(makeLenses ''State)

data Update = 
    Tick Word32 |
    MoveLeft |
    MoveRight |
    Rotate deriving Show

brickWidth :: Brick -> Int
brickWidth b = let
    arr = view (shape . unBrickShape) b
    in maximum (map length arr)

brickHeight :: Brick -> Int
brickHeight b = let
    arr = view (shape . unBrickShape) b
    in length arr

update :: Update -> State -> State
update (Tick newTick) = tick newTick
update MoveLeft = move (-1)
update MoveRight = move 1
update Rotate = rotate

rotate :: State -> State
rotate = over (active . brick . shape .unBrickShape) (map reverse . transpose)

move :: Int -> State -> State
move m s = let
    b = view (active . brick) s
    maxWidth = boardWidth - brickWidth b
    in over (active . x) (\n -> fit 0 (fromIntegral maxWidth) (n + m)) s

fit :: Ord a => a -> a -> a ->a
fit min max value
    | value < min = min
    | value < max = value
    | otherwise = max 

tick :: Word32 -> State -> State
tick newTick state = let
    b = view (active . brick) state
    bumpY = over (active . y) (\y -> fit 0 (boardHeight - brickHeight b) (y + 1))
    spaceBelow = canGoDownMore state
    updateFn = if spaceBelow then bumpY else newShape
    setTick = set lastTick newTick
      in (updateFn . setTick) state

newShape :: State -> State
newShape st = let
    cg = view currentGrid st
    a  = view active st
    newGrid = mergeBrickIntoGrid cg a
    in State {
        _active = ActiveBrick {
            _brick = Brick {
                _shape = head (view nextShapes st),
                _colour = head (view nextColours st)
            },
            _x = fromIntegral $ boardWidth `div` 2,
            _y = 0
        },
        _nextShapes = tail (view nextShapes st),
        _nextColours = tail (view nextColours st),
        _currentGrid = newGrid,
        _lastTick = (view lastTick st)
     }

mergeBrickIntoGrid :: [[Maybe Colour]] -> ActiveBrick -> [[Maybe Colour]]
mergeBrickIntoGrid original ab = let
    col = view (brick . colour) ab
    shp = view (brick . shape . unBrickShape) ab
    brk = view brick ab
    curx = view x ab
    maxx = curx + brickWidth brk
    cury = view y ab
    maxy = cury + brickHeight brk
    inrange xx yy = curx <= xx && xx < maxx && cury <= yy && yy < maxy
    inshape xx yy = shp !! (yy - cury) !! (xx - curx)
    in [
        [  if (inrange x y && inshape x y) then Just col else value |
            (value, x) <- zipWithIndex row
        ] |
        (row, y) <- zipWithIndex original
    ]
    

canGoDownMore :: State -> Bool
canGoDownMore st = let
    ypos = view (active . y) st
    bh = brickHeight (view (active . brick) st)
    in ypos + bh < boardHeight


boardHeight :: Int
boardHeight = 20

boardWidth :: Int
boardWidth = 10

squareSide :: Int
squareSide = 25

topMargin = 10 :: Int32

leftMargin = 10 :: Int32



square :: BrickShape
square = BrickShape [
    [True, True],
    [True, True]
    ]

leftSquiggle :: BrickShape
leftSquiggle = BrickShape [
     [False, True],
     [True, True],
     [True, False]
    ]   

rightSquiggle :: BrickShape
rightSquiggle = mirror leftSquiggle
    
leftL :: BrickShape
leftL = BrickShape [
    [False, True],
    [False, True],
    [True, True]
    ]

rightL :: BrickShape
rightL = mirror leftL    

mirror :: BrickShape -> BrickShape
mirror bs = BrickShape $ map reverse (_unBrickShape bs)

blackBackground :: V4 Word8
blackBackground = V4 0 0 0 0

whiteOutline :: V4 Word8
whiteOutline = V4 0xFF 0xFF 0xFF 0

emptyGrid :: [[Maybe a]]
emptyGrid = replicate (fromIntegral boardHeight) (replicate (fromIntegral boardWidth) Nothing) 

shapeSeq :: [BrickShape]
shapeSeq = cycle [square, leftSquiggle, rightSquiggle, leftL, rightL]

colourSeq :: [Colour]
colourSeq = cycle [Red, Green, Blue, Pink, Purple, Orange, White]

initialState :: Word32 -> State
initialState tick = State {
   _active = ActiveBrick {
       _brick = Brick {
           _shape = leftSquiggle,
           _colour = White
       },
       _x = fromIntegral $ boardWidth `div` 2,
       _y = 0
   },
   _nextShapes = shapeSeq,
   _nextColours = colourSeq,
   _currentGrid = emptyGrid,
   _lastTick = tick
}

renderColour :: Colour -> V4 Word8
renderColour Red = V4 0xFF 0 0 0
renderColour Blue = V4 0 0 0xFF 0
renderColour Green = V4 0 0xFF 0 0
renderColour White = V4 0xFF 0xFF 0xFF 0
renderColour _ = undefined

renderState :: State -> Renderer -> IO ()
renderState s r = do
    drawBackground
    drawOutline
    drawActive (view active s)
    drawGrid (view currentGrid s)
    present r
    where
        drawBackground = do
            rendererDrawColor r $= blackBackground
            clear r
        
        drawOutline = do
            rendererDrawColor r $= whiteOutline
            drawRect r $ Just $ mkRect leftMargin topMargin (fromIntegral (boardWidth * squareSide)) (fromIntegral (boardHeight * squareSide))

        drawSquare :: Int -> Int -> Colour -> IO ()    
        drawSquare x y col = do
            rendererDrawColor r $= renderColour col
            let topLeftX = leftMargin + fromIntegral (x * squareSide)
            let topLeftY = topMargin + fromIntegral (y * squareSide)
            fillRect r (Just $ mkRect topLeftX topLeftY (fromIntegral squareSide) (fromIntegral squareSide))

        drawActive :: ActiveBrick -> IO ()    
        drawActive activeBrick = do
            let col = view (brick . colour) activeBrick
            let shp = view (brick . shape . unBrickShape) activeBrick
            let shpDrawn = [ if isSet then drawSquare (xx + view x activeBrick) (yy + view y activeBrick) col else return () | (row, yy)  <- zipWithIndex shp, (isSet, xx) <- zipWithIndex row ] 
            sequence_ shpDrawn 

        drawGrid :: [[Maybe Colour]] -> IO ()
        drawGrid grid = sequence_ [ case v of
            Just col -> drawSquare x y col
            Nothing  -> return ()
            
            | (row, y) <- zipWithIndex grid, (v, x) <- zipWithIndex row]

mkRect :: Int32 -> Int32 -> Int32 -> Int32 -> Rectangle CInt
mkRect x y w h = Rectangle (P pt) vec
            where
                pt = V2 (CInt x) (CInt y)
                vec = V2 (CInt w) (CInt h)

zipWithIndex l = l `zip` [0..]                