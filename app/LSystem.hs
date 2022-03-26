{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (catMaybes)
import Control.Monad (forM_, foldM)
import Control.Monad.State (State(..), runState, modify, evalState, get, gets)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.HUnit

import Linear.V2
import Linear.V3
import Linear.Projection
import Linear.Matrix

import Text.Blaze.Svg11 ((!), mkPath, rotate, lr, mr, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

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

matchProduction :: [Production] -> Letter -> Production
matchProduction ps l =
  case filter (\p -> matchSymbol p == l) ps of
    [x] -> x
    []  -> identityProduction l
    _   -> error "unimplemented: multiple matching productions"

step :: LWord -> [Production] -> LWord
step (LWord axiom) productions =
  foldl (<>) mempty $ map (replacement . matchProduction productions) axiom

stepN 0 axiom _ = axiom
stepN n axiom rules = stepN (n - 1) (step axiom rules) rules


runSystem :: String -> Int -> Double -> String -> [(String, String)] -> IO ()

runSystem name n theta axiom ps =
  writeFile ("output/" <> name <> ".svg")
    $ generateSvg theta
    $ stepN n (lword axiom) (mkProductions ps)

--main = defaultMain tests
main = do
  runSystem "koch-island" 3 90.0 "F - F - F - F" [
      ("F", "F - F + F + F F - F - F + F")
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

data Instruction a = MovePenDown a | MovePenUp a

extrude :: Double -> (ProjectedPoint, ProjectedPoint) -> (ProjectedPoint, ProjectedPoint)
extrude t (V2 x1 y1, V2 x2 y2) =
  let
    sx = (x2 - x1) * t
    sy = (y2 - y1) * t
  in

    ( mkVec2 (x1 - sx) (y1 - sy), mkVec2 (x2 + sx) (y2 + sy))

generateSvg :: Double -> LWord -> String
generateSvg theta (LWord ls) = renderSvg svgDoc
  where
    thetaRads = theta / 180.0 * pi
    is = projectPathOrtho $ toPath thetaRads ls

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
            S.path ! A.d turtleToPath ! A.style "stroke-linecap:square;stroke:#50514F;stroke-width:0.2px;fill:none"

    turtleToPath :: S.AttributeValue
    turtleToPath = mkPath $ do
      m 0 0
      forM_ is $ \i ->
        case i of
          MovePenDown (V2 x y) -> lr x y
          MovePenUp (V2 x y) -> mr x y

bounds is =
  let (_, mn, mx) = foldl (\(
          pos,
          (V2 minX minY),
          (V2 maxX maxY)
        ) dv -> let
          (V2 x y) = pos
          (V2 dx dy) = dv
        in (
          pos + dv,
          mkVec2 (min minX (x + dx)) (min minY (y + dy)),
          mkVec2 (max maxX (x + dx)) (max maxY (y + dy))
         )) (mkVec2 0 0, mkVec2 0 0, mkVec2 0 0) (map toCoords is)
      in (mn, mx)

projectPathIso :: [Instruction Point] -> [Instruction ProjectedPoint]
projectPathIso = map f
  where
    f (MovePenDown x) = MovePenDown $ p x
    f (MovePenUp x) = MovePenDown $ p x
    p v3 = let (V3 x y _) = orthoProjection !* (isoProjection !* v3) in V2 x y

projectPathOrtho :: [Instruction Point] -> [Instruction ProjectedPoint]
projectPathOrtho = map f
  where
    f (MovePenDown x) = MovePenDown $ p x
    f (MovePenUp x) = MovePenDown $ p x
    p v3 = let (V3 x y _) = orthoProjection !* v3 in V2 x y

toCoords (MovePenDown c) = c
toCoords (MovePenUp c) = c

mkVec2 = V2
mkVec3 = V3

zeroV = V3 0 0 0

toPath thetaRads ls = catMaybes $ evalState (mapM f ls) [(270 / 180 * pi, zeroV)]
  where
    f :: Letter -> State [(Double, Point)] (Maybe (Instruction Point))
    f (Letter ('F':_)) = do
      heading <- gets (fst . head)
      let dv = mkVec3 (cos heading) (sin heading) 0
      modify (\((h, v):rest) -> ((h, v + dv):rest))
      return . Just $ MovePenDown dv
    f (Letter "f") = do
      heading <- gets (fst . head)
      let dv = mkVec3 (cos heading) (sin heading) 0
      modify (\((h, v):rest) -> ((h, v + dv):rest))
      return . Just $ MovePenUp dv
    f (Letter "[") = do
      modify (\(x:xs) -> (x:x:xs))
      return Nothing
    f (Letter "]") = do
      v1 <- gets (snd . head)
      modify (\(x:xs) -> xs)
      v2 <- gets (snd . head)

      return . Just $ MovePenUp (v2 - v1)
    f (Letter "+") = do
      modify (\((h, p):rest) -> ((h + thetaRads, p):rest))
      return Nothing
    f (Letter "-") = do
      modify (\((h, p):rest) -> ((h - thetaRads, p):rest))
      return Nothing
    f _ = return Nothing

mkProductions :: [(String, String)] -> [Production]
mkProductions template = map (\(l, w) -> Production {
  matchSymbol = Letter l,
  replacement = lword w
}) template

tests = testGroup "Deterministic & Context-Free (DOL)"
  [ testCase "Trivial" $ (lword "a b a b a a b a") @=? (stepN 5 (lword "b")
      $ mkProductions [
        ("b", "a"),
        ("a", "b a")
      ])
  , testCase "Anabaena catenula" $ (lword "b◀ a▶ b◀ a▶ a▶ b◀ a▶ a▶") @=?
      (stepN 4 (lword "a▶")
        $ mkProductions [
          ("a▶", "a◀ b▶"),
          ("a◀", "b◀ a▶"),
          ("b▶", "a▶"),
          ("b◀", "a◀")
        ])
  , testCase "Koch island"
      $ (lword "F - F + F + F F - F - F + F - F - F + F + F F - F - F + F - F - F + F + F F - F - F + F - F - F + F + F F - F - F + F") @=?
      (stepN 1 (lword "F - F - F - F")
        $ mkProductions [
          ("F", "F - F + F + F F - F - F + F")
        ])
  , testCase "Bracketed"
      $ (lword "F [ + F ] F [ - F ] F") @=?
      (stepN 1 (lword "F")
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
