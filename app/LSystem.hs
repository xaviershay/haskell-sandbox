{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (catMaybes)
import Control.Monad (forM_, foldM)
import Control.Monad.State (State(..), runState, modify, evalState, get, gets)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.HUnit

import Text.Blaze.Svg11 ((!), mkPath, rotate, lr, mr, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

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

type Point = (Double, Double)

data Instruction = MovePenDown Point | MovePenUp Point

extrude :: Double -> (Point, Point) -> (Point, Point)
extrude t ((x1, y1), (x2, y2)) =
  let
    sx = (x2 - x1) * t
    sy = (y2 - y1) * t
  in

    ( (x1 - sx, y1 - sy), (x2 + sx, y2 + sy))

generateSvg :: Double -> LWord -> String
generateSvg theta (LWord ls) = renderSvg svgDoc
  where
    thetaRads = theta / 180.0 * pi
    is = toPath thetaRads ls

    svgDoc :: S.Svg
    svgDoc =
      let
        ((minX, minY), (maxX, maxY)) = extrude 0.1 (bounds is)
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
          MovePenDown (x, y) -> lr x y
          MovePenUp (x, y) -> mr x y

bounds is =
  let (_, mn, mx) = foldl (\(
          (x, y),
          (minX, minY),
          (maxX, maxY)
        ) (dx, dy) -> (
          (x + dx, y + dy),
          (min minX (x + dx), min minY (y + dy)),
          (max maxX (x + dx), max maxY (y + dy))
         )) ((0,0),(0,0), (0, 0)) (map toCoords is)
      in (mn, mx)

toCoords (MovePenDown c) = c
toCoords (MovePenUp c) = c

toPath thetaRads ls = catMaybes $ evalState (mapM f ls) [(270 / 180 * pi, (0,0))]
  where
    f :: Letter -> State [(Double, (Double, Double))] (Maybe Instruction)
    f (Letter ('F':_)) = do
      heading <- gets (fst . head)
      modify (\((h, (x, y)):rest) -> ((h, (x + cos heading, y + sin heading)):rest))
      return . Just $ MovePenDown (cos heading, sin heading)
    f (Letter "f") = do
      heading <- gets (fst . head)
      modify (\((h, (x, y)):rest) -> ((h, (x + cos heading, y + sin heading)):rest))
      return . Just $ MovePenUp (cos heading, sin heading)
    f (Letter "[") = do
      modify (\(x:xs) -> (x:x:xs))
      return Nothing
    f (Letter "]") = do
      (x1, y1) <- gets (snd . head)
      modify (\(x:xs) -> xs)
      (x2, y2) <- gets (snd . head)

      return . Just $ MovePenUp (x2 - x1, y2 - y1)
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
