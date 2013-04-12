{-# LANGUAGE FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}

module Main where

import Data.List
import System.Process
import System.Console.ANSI(clearScreen)
import Control.Concurrent
import Control.Concurrent.Timer
import Control.Concurrent.MVar
import Control.Concurrent.Suspend.Lifted(msDelay)
import System.IO
import Control.Exception(bracket)
import Control.Applicative
import Control.Monad
import System.Random


main = do
    let h = 30
    let w = 60

    let randGen = mkStdGen 1

    dirVar <- newMVar (0,0)
    posVar <- newMVar (0,0)
    trackVar <- newMVar (track randGen w)
    t <- repeatedTimer (updateScreen randGen posVar dirVar trackVar w h) (msDelay 50)        
    forever $ handleKeyboard dirVar t w h


updateScreen randGen posv dirv trv w h = do
                (dx,dy) <- takeMVar dirv
                (x,y) <- takeMVar posv
                let x' = clamp 0 (w-1) (x+dx)
                let y' = clamp 0 (h-1) (y+dy)
                putMVar posv (x', y')
                putMVar dirv (0, 0)
                drawPos randGen x' y' trv h


handleKeyboard dirv t w h = withHiddenTerminalInput $ do        
                key <- getChar
                (dx,dy) <- takeMVar dirv
                
                let clamp' = clamp (-1) 1
 
                if key == 'q' 
                    then stopTimer t
                    else return ()

                putMVar dirv $
                    if key == 'w' then (dx, clamp' (dy-1))
                        else if key == 's' then (dx, clamp' (dy+1))
                        else if key == 'a' then (clamp' (dx-1), dy)
                        else if  key == 'd' then (clamp' (dx+1), dy)
                        else (dx,dy)
                            


clamp :: Ord a => a -> a -> a -> a
clamp min max a = if a < min then min
                else if a > max then max
                else a

type Width = Int
type Height = Int
type Seed = Int
    {-
terrain :: ((Int,Int) -> Char) -> (Width, Height) -> [String]
terrain f = map[0..]
-}

data TrackSection = TrackSection { tsOffset :: Int, tsWidth :: Int }


pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp [a] = []
pairUp (a:b:rest) = (a,b) : pairUp rest

         

track :: RandomGen g => g -> Width -> [String]
track g w = map trackSlice $ zip (trackPath g w) (randoms g)
    where
        -- terrain = randomTerrain
        trackSlice ((offset, width), seed) = take left landTerrain
                            ++ take trW roadTerrain
                            ++ (take right . drop (left+trW) $ landTerrain)
            where
                trW = clamp 10 20 width
                left = clamp 0 (w-trW) offset
                right = w - left - trW
                roadTerrain = repeat ' '
                landTerrain = terrain nextGen w
                (_,nextGen) = next (mkStdGen seed)


trackPath :: RandomGen g => g -> Width -> [(Int,Int)]
trackPath g width = parts
    where
        parts = first : zipWith offsetPrev parts diffs
        offsetPrev (o,w) (d1,d2) = (clamp 0 (width - w - d2) (o + d1), w + d2)
        -- always start 20 chars wide, in the middle
        first = ((width-20) `div` 2, 20 ) --TrackSection { tsOffset = (w-6) `div` 2, tsWidth = 6 }
        diffs = randomRs ((-1,-1),(1,1)) $ g


drawPos randGen x y trv h = do
    clearScreen
    tr <- drop 1 <$> takeMVar trv
    let canvas = update x y 'O' (take h tr)
    putMVar trv tr
    putStrLn "\n"
    mapM_ putStrLn canvas


update :: Int -> Int -> Char -> [String] -> [String]
update x y c arr = take y arr ++ updateLine x c (arr!!y) : drop (y+1) arr
    where updateLine x v line = take x line ++ v : drop (x+1) line


terrain :: RandomGen g => g -> Width -> [Char]
terrain g w = tile : terrain g' w
    where
        (n, g') = randomR (0, 40::Int) g
        tile = if n < 1 then 'T' else '.'

instance (Random a, Random b) => Random (a,b) where
    random g = let (x, g') = random g
                   (y, g'') = random g'
               in ((x, y), g'')
    randomR ((m1, m2), (n1,n2)) g = 
                let (x, g') = randomR (m1,n1) g
                    (y, g'') = randomR (m2,n2) g'
                in ((x, y), g'')



-- Thanks to StackExchange
withHiddenTerminalInput :: IO a -> IO a
withHiddenTerminalInput io = bracket
   (do prevBuff <- hGetBuffering stdin
       prevEcho <- hGetEcho stdin

       hSetBuffering stdin NoBuffering
       hSetEcho stdin False

       return (prevBuff, prevEcho)
   )

   (\(prevBuff, prevEcho) -> do
       hSetBuffering stdin prevBuff
       hSetEcho stdin prevEcho
   )

   (const io)

