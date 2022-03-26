{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (catMaybes)
import Control.Monad (forM_, foldM)
import Control.Monad.State (State(..), runState, modify, evalState, get)
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

--main = defaultMain tests
main = do
  writeFile "output/koch-island.svg" $ generateSvg 0.09 (20, 80) (90.0) $
    stepN 3 (lword "F - F - F - F") $ mkProductions [
        ("F", "F - F + F + F F - F - F + F")
    ]

  writeFile "output/islands-lakes.svg" $ generateSvg 0.15 (20, 20) (90.0) $
      stepN 2 (lword "F + F + F + F") $ mkProductions [
          ("F", "F + f - F F + F + F F + F f + F F - f + F F - F - F F - F f - F F F")
        ]

data Instruction = MovePenDown (Double, Double) | MovePenUp (Double, Double)

generateSvg :: Double -> (Double, Double) -> Double -> LWord -> String
generateSvg scale (ox, oy) theta (LWord ls) = renderSvg svgDoc
  where
    thetaRads = theta / 180.0 * pi
    svgDoc :: S.Svg
    svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" ! A.viewbox "0 0 100 100" $ do
        S.g $ do
          S.rect ! A.width "100" ! A.height "100" ! A.fill "#CBD4C2"
        S.g $ do
          S.path ! A.d turtleToPath ! A.style "stroke:#50514F;stroke-width:0.2;fill:none"

    turtleToPath :: S.AttributeValue
    turtleToPath  = mkPath $ do
      let s = scale * 10
      m ox oy
      forM_ (toPath ls) $ \i ->
        case i of
          MovePenDown (x, y) -> lr (x * s) (y * s)
          MovePenUp (x, y) -> mr (x * s) (y * s)

    toPath ls = catMaybes $ evalState (mapM f ls) 0

    f :: Letter -> State Double (Maybe Instruction)
    f (Letter "F") = do
      heading <- get
      return . Just $ MovePenDown (cos heading, sin heading)
    f (Letter "f") = do
      heading <- get
      return . Just $ MovePenUp (cos heading, sin heading)
    f (Letter "+") = do
      modify ((+) thetaRads)
      return Nothing
    f (Letter "-") = do
      modify (\x -> x - thetaRads)
      return Nothing

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
