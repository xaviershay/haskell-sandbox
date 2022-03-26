{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace
import Data.Maybe (catMaybes)
import Control.Monad (forM_, foldM, guard)
import Control.Monad.State (State(..), runState, modify, evalState, get, gets)
import Data.List (intercalate)
import Data.Monoid ((<>))
import System.Random
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.HUnit

import Linear.V2
import Linear.V3
import Linear.Projection
import Linear.Matrix

import Text.Blaze.Svg11 ((!), mkPath, rotate, lr, mr, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
--import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)

isoProjection = (1 / sqrt 6) * V3
  (V3 (sqrt 3) 0 ((-1) * sqrt 3))
  (V3 1 2 1)
  (V3 (sqrt 2) ((-1) * sqrt 2) (sqrt 2))

orthoProjection = V3
  (V3 1.0 0 0)
  (V3 0.0 1 0)
  (V3 0.0 0 0)

data Letter = Letter {
  symbol :: String
} deriving (Eq)

instance Show Letter where
  show (Letter x) = x

data LWord = LWord [Letter]
  deriving (Eq)

instance Semigroup LWord where
  (LWord a) <> (LWord b) = LWord $ a <> b
instance Monoid LWord where
  mempty = LWord []

instance Show LWord where
  show (LWord l) = intercalate " " $ map show l

data Production = Production {
  matchSymbol :: Letter,
  replacement :: LWord
} deriving (Show)

lword = LWord . map Letter . words

identityProduction l = Production { matchSymbol = l, replacement = LWord [l] }

matchProduction :: StatefulGen g m => [Production] -> g -> Letter -> m Production
matchProduction ps gen l  =
  case filter (\p -> matchSymbol p == l) ps of
    [x] -> return $ x
    []  -> return $ identityProduction l
    xs  -> do
      i <- uniformRM (0, length xs - 1) gen
      return $ xs !! i

step :: StatefulGen g m => LWord -> [Production] -> g -> m LWord
step (LWord axiom) productions gen = do
  parts <- mapM (matchProduction productions gen) axiom
  return $ foldl (<>) mempty $ map replacement parts

stepNM :: StatefulGen g m => Int -> LWord -> [Production] -> g -> m LWord
stepNM 0 axiom _ _ = return axiom
stepNM n axiom rules gen = do
  word <- step axiom rules gen

  stepNM (n - 1) word rules gen

stepN :: RandomGen g => g -> Int -> LWord -> [Production] -> LWord
stepN gen n axiom rules = fst $ runStateGen gen (stepNM n axiom rules)

runSystem :: String -> Int -> Double -> String -> [(String, String)] -> IO ()
runSystem name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg")
    $ generateSvg projectPathOrtho theta
    $ stepN gen n (lword axiom) (mkProductions ps)

runSystem3D :: String -> Int -> Double -> String -> [(String, String)] -> IO ()
runSystem3D name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg")
    $ generateSvg projectPathIso theta
    $ stepN gen n (lword axiom) (mkProductions ps)

--main = defaultMain tests
main = do
  runSystem "stochastic" 5 20 "S"
    [ ("S", "S [ / / & & L ] [ / / ^ ^ L ] F S")
    , ("S", "S F S")
    , ("S", "S")
    , ("L", "[ ' F ]")
    ]

  guard False
  runSystem3D "plant" 4 18 "P"
    [ ("P", "I + [ P + Fl ] - - / / [ - - L ] I [ + + L ] - [ P Fl ] + + P Fl")
    , ("I", "F S [ / / & & L ] [ / / ^ ^ L ] F S")
    , ("S", "S F S")
    --, ("L", "[ ' { + F - F F - F + | + F - F F - F } ]")
    , ("L", "[ ' { + F - F - F + | + F - F - F } ]")
    , ("Fl", "[ & & & C ' / W / / / / W / / / / W / / / / W / / / / W ]")
    , ("C", "F F")
    , ("W", "[ ' ^ F ] [ { & & & & - F + F | - F + F } ]")
    ]

  runSystem "koch-island" 3 90.0 "F - F - F - F" [
      ("F", "F - F + F + F F - F - F + F")
    ]

  runSystem3D "hilbert" 3 90 "A" [
      ("A", "B - F + C F C + F - D & F ^ D - F + & & C F C + F + B / /"),
      ("B", "A & F ^ C F B ^ F ^ D ^ ^ - F - D ^ | F ^ B | F C ^ F ^ A / /"),
      ("C", "| D ^ | F ^ B - F + C ^ F ^ A & & F A & F ^ C + F + B ^ F ^ D / /"),
      ("D", "| C F B - F + B | F A & F ^ A & & F B - F + B | F C / /")
    ]

  runSystem "islands-lakes" 2 90.0 "F + F + F + F" [
      ("F", "F + f - F F + F + F F + F f + F F - f + F F - F - F F - F f - F F F")
    ]

  runSystem "koch-tiles" 3 90.0 "F - F - F - F" [
      ("F", "F F - F + F - F - F F")
    ]

  runSystem "koch-spiral" 4 90.0 "F - F - F - F" [
      ("F", "F - F + F - F - F")
    ]

  runSystem "dragon-curve" 9 90.0 "F◀" [
      ("F◀", "F◀ + F▶ +"),
      ("F▶", "- F◀ - F▶")
    ]

  runSystem "gosper-hex-curve" 4 60.0 "FL" [
      ("FL", "FL + FR + + FR - FL - - FL FL - FR +"),
      ("FR", "- FL + FR FR + + FR + FL - - FL - FR")
    ]

  runSystem "branching-1" 4 25.7 "F" [
      ("F", "F [ + F ] F [ - F ] F")
    ]

  runSystem "branching-2" 4 20 "F" [
      ("F", "F [ + F ] F [ - F ] [ F ]")
    ]

  runSystem "branching-3" 4 22.5 "F" [
      ("F", "F F - [ - F + F + F ] + [ + F - F - F ]")
    ]

  runSystem "branching-4" 7 20 "X" [
      ("X", "F [ + X ] F [ - X ] + X"),
      ("F", "F F")
    ]

  runSystem "branching-5" 6 25.7 "X" [
      ("X", "F [ + X ] [ - X ] F X"),
      ("F", "F F")
    ]

  runSystem "branching-6" 5 22.5 "X" [
      ("X", "F - [ [ X ] + X ] + F [ + F X ] - X"),
      ("F", "F F")
    ]

type Point = V3 Double
type ProjectedPoint = V2 Double

data Instruction a = MovePenDown a | MovePenUp a | ChangeColor Int deriving (Show)

colors :: [String]
colors =
  [ "#50514F"
  , "#109648"
  , "#CB793A"
  , "#8D91C7"
  , "#000000"
  , "#ffff00"
  ]

extrude :: Double -> (ProjectedPoint, ProjectedPoint) -> (ProjectedPoint, ProjectedPoint)
extrude t (V2 x1 y1, V2 x2 y2) =
  let
    sx = (x2 - x1) * t
    sy = (y2 - y1) * t
  in

    ( mkVec2 (x1 - sx) (y1 - sy), mkVec2 (x2 + sx) (y2 + sy))

generateSvg projectF theta (LWord ls) = renderSvg svgDoc
  where
    thetaRads = theta / 180.0 * pi
    unprojected = toPath thetaRads ls
    is = projectF unprojected
    --is = projectF unprojected

    svgDoc :: S.Svg
    svgDoc =
      let
        (V2 minX minY, V2 maxX maxY) = extrude 0.1 (bounds is)
        aspect = (maxX - minX) / (maxY - minY)
      in
      S.docTypeSvg
        ! A.version "1.1"
        ! A.width (S.toValue $ 500 * aspect)
        ! A.height "500"
        ! A.viewbox (S.toValue . intercalate " " . map show $ [minX, minY, maxX - minX, maxY - minY])
        $ do
          S.g $ do
            S.rect ! A.x (S.toValue minX) ! A.y (S.toValue minY) ! A.width (S.toValue $ maxX - minX) ! A.height (S.toValue $ maxY - minY) ! A.fill "#CBD4C2"
            forM_ (t is) $ \(colorIndex, is') -> do
              S.path
                ! A.style (S.toValue $ "stroke-linecap:square;stroke-width:0.1px;fill:none;stroke:" <> (colors !! (colorIndex `mod` length colors)))
                ! A.d (mkPath $ do
                    let (MovePenUp (V2 x y):t) = reverse is'
                    m x y
                    forM_ t $ \i ->
                      case i of
                        MovePenDown (V2 x y) -> l x y
                        MovePenUp (V2 x y) -> m x y)
                

    turtleToPath :: S.AttributeValue
    turtleToPath = mkPath $ do
      m 0 0
      forM_ is $ \i ->
        case i of
          MovePenDown (V2 x y) -> lr x y
          MovePenUp (V2 x y) -> mr x y
          ChangeColor _ -> return ()

t :: [Instruction ProjectedPoint] -> [(Int, [Instruction ProjectedPoint])]
t is = reverse $ foldl f ([(0, [MovePenUp (V2 0 0)])]) is
  where
    initPath ((MovePenDown v):_) = [MovePenUp v]
    initPath ((MovePenUp v):_) = [MovePenUp v]
    initPath _ = []
    f ((style, is):rest) i = case i of
                                  ChangeColor s -> (s, initPath is):(style, is):rest
                                  x -> (style, x:is):rest

bounds is =
  let (mn, mx) = foldl (\(
          (V2 minX minY),
          (V2 maxX maxY)
        ) pos -> let
          (V2 x y) = pos
        in (
          mkVec2 (min minX x) (min minY y),
          mkVec2 (max maxX x) (max maxY y)
         )) (mkVec2 0 0, mkVec2 0 0) (map toCoords is)
      in (mn, mx)

projectPathIso :: [Instruction Point] -> [Instruction ProjectedPoint]
projectPathIso = map f
  where
    f (MovePenDown x) = MovePenDown $ p x
    f (MovePenUp x) = MovePenUp $ p x
    f (ChangeColor x) = ChangeColor x
    p v3 = let (V3 x y _) = orthoProjection !* (isoProjection !* v3) in V2 x y

projectPathOrtho :: [Instruction Point] -> [Instruction ProjectedPoint]
projectPathOrtho = map f
  where
    f (MovePenDown x) = MovePenDown $ p x
    f (MovePenUp x) = MovePenUp $ p x
    f (ChangeColor x) = ChangeColor x
    p v3 = let (V3 x y _) = orthoProjection !* v3 in V2 x y

toCoords (MovePenDown c) = c
toCoords (MovePenUp c) = c
toCoords _ = (mkVec2 0 0)

mkVec2 = V2
mkVec3 = V3

zeroV = V3 0 0 0

rotateU a = V3
  (V3 (cos a) (sin a) 0)
  (V3 ((-1) * (sin a)) (cos a) 0)
  (V3 0 0 1)

rotateL a = V3
  (V3 (cos a) 0 (sin a * (-1)))
  (V3 0 1 0)
  (V3 (sin a) 0 (cos a))

rotateH a = V3
  (V3 1 0 0)
  (V3 0 (cos a) (sin a * (-1)))
  (V3 0 (sin a) (cos a))

data TurtleState = TurtleState {
  rotateM :: V3 Point,
  position :: Point,
  color :: Int
}

initialTurtle = TurtleState {
  rotateM = rotateU $ 90 / 180 * pi,
  position = V3 0 0 0,
  color = 0
}
 
toPath thetaRads ls = concat . catMaybes $ evalState (mapM f ls) [initialTurtle]
  where
    f :: Letter -> State [TurtleState] (Maybe [Instruction Point])
    f (Letter ('F':_)) = do
      rv <- gets (rotateM . head)
      let dv = rv !* (mkVec3 1 0 0)
      modifyP dv
      v <- gets (position . head)
      return . Just $ [MovePenDown v]
    f (Letter "f") = do
      rv <- gets (rotateM . head)
      let dv = rv !* (mkVec3 1 0 0)
      modifyP dv
      v <- gets (position . head)
      return . Just $ [MovePenUp v]
    f (Letter "[") = do
      modify (\(x:xs) -> (x:x:xs))
      return Nothing
    f (Letter "]") = do
      modify (\(x:xs) -> xs)
      v2 <- gets (position . head)
      c <- gets (color . head)

      return . Just $ [MovePenUp v2, ChangeColor c]
    f (Letter "+") = do
      modifyR (rotateU thetaRads)
      return Nothing
    f (Letter "-") = do
      modifyR (rotateU (-thetaRads))
      return Nothing
    f (Letter "&") = do
      modifyR (rotateL thetaRads)
      return Nothing
    f (Letter "^") = do
      modifyR (rotateL (-thetaRads))
      return Nothing
    f (Letter "\\") = do
      modifyR (rotateH thetaRads)
      return Nothing
    f (Letter "/") = do
      modifyR (rotateH (-thetaRads))
      return Nothing
    f (Letter "|") = do
      modifyR (rotateU pi)
      return Nothing
    f (Letter "'") = do
      modifyC (+ 1)
      c <- gets (color . head)
      return . Just $ [ChangeColor c]
    f _ = return Nothing
    -- f unknown = error $ "unimplemented: " <> show unknown

modifyP m = modify (\(t:ts) -> (t { position = position t + m }:ts))
modifyR m = modify (\(t:ts) -> (t { rotateM = rotateM t !*! m }:ts))
modifyC m = modify (\(t:ts) -> (t { color = m (color t) }:ts))

mkProductions :: [(String, String)] -> [Production]
mkProductions template = map (\(l, w) -> Production {
  matchSymbol = Letter l,
  replacement = lword w
}) template

tests = let gen = mkStdGen 42 in testGroup "Deterministic & Context-Free (DOL)"
  [ testCase "Trivial" $ (lword "a b a b a a b a") @=? (stepN gen 5 (lword "b")
      $ mkProductions [
        ("b", "a"),
        ("a", "b a")
      ])
  , testCase "Anabaena catenula" $ (lword "b◀ a▶ b◀ a▶ a▶ b◀ a▶ a▶") @=?
      (stepN gen 4 (lword "a▶")
        $ mkProductions [
          ("a▶", "a◀ b▶"),
          ("a◀", "b◀ a▶"),
          ("b▶", "a▶"),
          ("b◀", "a◀")
        ])
  , testCase "Koch island"
      $ (lword "F - F + F + F F - F - F + F - F - F + F + F F - F - F + F - F - F + F + F F - F - F + F - F - F + F + F F - F - F + F") @=?
      (stepN gen 1 (lword "F - F - F - F")
        $ mkProductions [
          ("F", "F - F + F + F F - F - F + F")
        ])
  , testCase "Bracketed"
      $ (lword "F [ + F ] F [ - F ] F") @=?
      (stepN gen 1 (lword "F")
      $ mkProductions [
        ("F", "F [ + F ] F [ - F ] F")
      ])
  ]

makePath :: S.AttributeValue
makePath = mkPath $ do
  m 0 0
  l 2 3
  l 4 5

makeTransform :: S.AttributeValue
makeTransform = rotate 50
