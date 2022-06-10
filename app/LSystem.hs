{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM_, guard, when)
import Control.Monad.RWS hiding (Product, Sum)
import Control.Monad.State (State(..), evalState, get, gets, modify, runState)
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.List (groupBy, intercalate, partition, tails)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Debug.Trace
import Numeric
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Random
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Parsec as P
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Tok
import qualified Text.Parsec.Token as Tok

import Linear.Matrix
import Linear.Metric
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.Vector

import Text.Blaze.Svg11 ((!), l, lr, m, mkPath, mr, rotate)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

--import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      Tok.emptyDef
        { Tok.reservedOpNames = ["+", "^", "/", "*"]
        , Tok.reservedNames = []
        , Tok.identStart = P.letter
        }

reservedOp = Tok.reservedOp lexer

whiteSpace = Tok.whiteSpace lexer

parens = P.between (P.char '(' <* whiteSpace) (P.char ')')

termExpr =
  (parens exprParser <|> ExprConst <$> paramExpr <|>
   ExprVar <$> P.many1 exprSymbolExpr) <*
  whiteSpace

prefix name fun =
  Prefix
    (do reservedOp name
        return fun)

binary name fun assoc =
  Infix
    (do reservedOp name
        return fun)
    assoc

table =
  [ [prefix "-" (ExprOp2 Product (ExprConst (-1)))]
  , [binary "^" (ExprOp2 Exponent) AssocLeft]
  , [ binary "*" (ExprOp2 Product) AssocLeft
    , binary "/" (ExprOp2 Fraction) AssocLeft
    ]
  , [ binary "+" (ExprOp2 Sum) AssocLeft
    , binary
        "-"
        (\a b -> ExprOp2 Sum a (ExprOp2 Product b (ExprConst (-1))))
        AssocLeft
    ]
  ]

exprParser = buildExpressionParser table (whiteSpace *> termExpr)

exprSymbolExpr = P.satisfy (\x -> not $ x `elem` (" (),+-*/^" :: String))

symbolExpr = P.satisfy (\x -> not $ x `elem` (" ()," :: String))

paramExpr = do
  leading <- P.many1 P.digit
  decimal <- P.optionMaybe (P.char '.' >> P.many1 P.digit)
  let trailing =
        case decimal of
          Just x -> "." <> x
          Nothing -> ""
  return $ read (leading <> trailing)

patternExpr = P.many1 symbolExpr

letterExpr paramParser = do
  symbol <- P.many1 symbolExpr
  params <-
    P.try (P.char '(' >> whiteSpace) *>
    (paramParser <* whiteSpace) `P.sepBy` (P.char ',' >> whiteSpace) <*
    (P.char ')') <|>
    pure []
  return $ PLetter {letterSymbol = symbol, letterParams = params}

pwordExpr =
  LWord <$> (whiteSpace *> P.many1 (letterExpr paramExpr <* whiteSpace))

pwordExprExpr =
  LWord <$> (whiteSpace *> P.many (letterExpr exprParser <* whiteSpace))

pwordPatternExpr = whiteSpace *> letterExpr patternExpr <* whiteSpace

guardParser = do
  lhs <- exprParser
  operator <- foldl1 (<|>) $ map (P.try . P.string) ["=", "<=", ">=", "<", ">"]
  rhs <- exprParser
  return $ MatchGuard operator lhs rhs

parseUnsafe :: String -> LWord Letter
parseUnsafe input =
  case P.parse pwordExpr input input of
    Right x -> x
    Left x -> error $ show x

parseUnsafePattern :: String -> LetterPattern
parseUnsafePattern input =
  case P.parse pwordPatternExpr input input of
    Right x -> x
    Left x -> error $ show x

parseUnsafeWordExpr :: String -> LWord LetterExpr
parseUnsafeWordExpr input =
  case P.parse pwordExprExpr input input of
    Right x -> x
    Left x -> error $ show x

parseUnsafeExpr :: String -> Expr
parseUnsafeExpr input =
  case P.parse exprParser input input of
    Right x -> x
    Left x -> error $ show x

parseUnsafeGuard :: String -> MatchGuard
parseUnsafeGuard input =
  case P.parse (guardParser <|> pure MatchAll) input input of
    Right x -> x
    Left x -> error $ show x

-- First transform the X/Y axis to point R/U (rather than default R/D in SVG
-- coords), then apply standard iso projection rotations.
isoProjection = rotateH beta !*! rotateL (pi / 4) !*! rotateL pi !*! rotateU pi

beta = asin (tan (30.0 / 180.0 * pi))

-- Flip Y axis such that positive direction is up rather than down
orthoProjection = V3 (V3 1.0 0 0) (V3 0.0 (-1) 0) (V3 0.0 0 0)

data PLetter a = PLetter
  { letterSymbol :: String
  , letterParams :: [a]
  } deriving (Eq)

plword s = parseUnsafe s

data Term2
  = Sum
  | Product
  | Fraction
  | Exponent
  deriving (Show, Eq)

data Env =
  Env (M.HashMap String Expr)
  deriving (Show)

instance Semigroup Env where
  Env a <> Env b = Env (a <> b)

instance Monoid Env where
  mempty = Env mempty

data Expr
  = ExprConst Double
  | ExprVar String
  | ExprOp2 Term2
            Expr
            Expr
  deriving (Show, Eq)

type LetterExpr = PLetter Expr

type Letter = PLetter Double

type LetterPattern = PLetter String

letter :: String -> Letter
letter a = PLetter {letterSymbol = a, letterParams = mempty}

instance Show a => Show (PLetter a) where
  show (PLetter {letterSymbol = s, letterParams = ps}) =
    s <>
    if null ps
      then ""
      else show ps

data LWord a =
  LWord [a]
  deriving (Eq)

instance Semigroup (LWord x) where
  (LWord a) <> (LWord b) = LWord $ a <> b

instance Monoid (LWord a) where
  mempty = LWord []

instance Show a => Show (LWord a) where
  show (LWord l) = intercalate " " $ map show l

data Production = Production
  { prodRule :: MatchRule
  , replacement :: LWord (LetterExpr)
  } deriving (Show)

data MatchRule = MatchRule
  { ruleLetter :: LetterPattern
  , ruleLetterPre :: Maybe LetterPattern
  , ruleLetterPost :: Maybe LetterPattern
  , ruleGuard :: MatchGuard
  } deriving (Show)

data MatchGuard
  = MatchAll
  | MatchGuard String
               Expr
               Expr
  deriving (Show, Eq)

match = mkRule

mkRule l =
  MatchRule
    { ruleLetter = parseUnsafePattern l
    , ruleLetterPre = Nothing
    , ruleLetterPost = Nothing
    , ruleGuard = MatchAll
    }

applyRule :: Env -> Production -> LetterContext -> Bool
applyRule globalEnv prod context@((l, pre), post) =
  let r = prodRule prod
   in letterSymbol (ruleLetter r) == letterSymbol l &&
      case ruleLetterPre r of
        Nothing -> True
        Just _ -> fmap letterSymbol (ruleLetterPre r) == fmap letterSymbol pre &&
      case ruleLetterPost r of
        Nothing -> True
        Just _ -> fmap letterSymbol (ruleLetterPost r) == fmap letterSymbol post &&
      case ruleGuard r of
        MatchAll -> True
        MatchGuard op lhs rhs ->
          let env = globalEnv <> envFromContext prod context
              lhs' = eval env lhs
              rhs' = eval env rhs
           in case op of
                "=" -> lhs' == rhs'
                "<" -> lhs' < rhs'
                ">" -> lhs' > rhs'
                "<=" -> lhs' <= rhs'
                ">=" -> lhs' >= rhs'
                x -> error $ "undefined operator: " <> x

l <| r = r {ruleLetterPre = Just (parseUnsafePattern l)}

r |> l = r {ruleLetterPost = Just (parseUnsafePattern l)}

(|:) :: MatchRule -> String -> MatchRule
rule |: guardString = rule {ruleGuard = parseUnsafeGuard guardString}

lword :: String -> LWord Letter
lword = LWord . map letter . words

type LetterContext = ((Letter, Maybe Letter), Maybe Letter)

identityProduction :: LetterContext -> Production
identityProduction ((l, _), _) =
  Production
    { prodRule = mkRule (letterSymbol l)
    , replacement =
        LWord
          [ PLetter
              { letterSymbol = letterSymbol l
              , letterParams = map ExprConst $ letterParams l
              }
          ]
    }

envFromContext :: Production -> LetterContext -> Env
envFromContext p ((l1, l2), l3) =
  Env . M.fromList $
  envForLetter (Just $ ruleLetter rule) (Just l1) <>
  envForLetter (ruleLetterPre rule) l2 <>
  envForLetter (ruleLetterPost rule) l3
  where
    rule = prodRule p
    envForLetter :: Maybe LetterPattern -> Maybe Letter -> [(String, Expr)]
    envForLetter pat l =
      case (pat, l) of
        (Just p', Just l') ->
          let paramLabels = letterParams p'
              paramValues = map ExprConst . letterParams $ l'
           in zip paramLabels paramValues
        _ -> []

emptyEnv = Env mempty

hasGuard :: Production -> Bool
hasGuard = (/= MatchAll) . ruleGuard . prodRule

matchProduction ::
     StatefulGen g m => Productions -> g -> LetterContext -> m (Production, Env)
matchProduction ps gen context =
  let buildEnv x = prodsDefines ps <> envFromContext x context
   in case filter (\p -> applyRule (prodsDefines ps) p context) (prodsRules ps) of
        [x] ->
          return $
          let y = (x, buildEnv x)
           in y
        [] -> return $ (identityProduction context, emptyEnv)
        xs ->
          case partition hasGuard xs of
            ([], xs') -> do
              i <- uniformRM (0, length xs' - 1) gen
              return $
                let x = xs' !! i
                 in (x, buildEnv x)
            (xs', _) -> do
              i <- uniformRM (0, length xs' - 1) gen
              return $
                let x = xs' !! i
                 in (x, buildEnv x)

type Projection = [Instruction Point] -> [Instruction ProjectedPoint]

data Viewport
  = ViewportFixed (ProjectedPoint, ProjectedPoint)
  | ViewportBoundingRect Double

data Picture = Picture
  { pictureTitle :: String
  , pictureAxiom :: LWord Letter
  , pictureN :: Int
  , pictureColors :: [String]
  , pictureStrokeWidth :: Double
  , pictureTheta :: Double
  , pictureProductions :: Productions
  , pictureSeed :: Int
  , pictureProjection :: Projection
  , pictureDebug :: Bool
  , pictureViewport :: Viewport
  }

emptyPicture =
  Picture
    { pictureTitle = ""
    , pictureAxiom = parseUnsafe ""
    , pictureN = 0
    , pictureColors = defaultColors
    , pictureStrokeWidth = defaultStrokeWidth
    , pictureTheta = 90
    , pictureProductions = emptyProductions
    , pictureSeed = 0
    , pictureProjection = projectPathOrtho
    , pictureDebug = False
    , pictureViewport = ViewportBoundingRect 0.1
    }

data Productions = Productions
  { prodsRules :: [Production]
  , prodsIgnore :: [Letter]
  , prodsDefines :: Env
  }

emptyProductions =
  Productions {prodsRules = mempty, prodsIgnore = mempty, prodsDefines = mempty}

headMaybe (x:_) = Just x
headMaybe [] = Nothing

extractPosts word ignores =
  let f = filter (\x -> not $ letterSymbol x `elem` (map letterSymbol ignores))
   in map (headMaybe . f) . drop 1 . tails $ word

extractPres word = reverse . extractPosts (reverse word)

evalExpr :: LWord LetterExpr -> LWord Letter
evalExpr (LWord ls) = LWord . map f $ ls
  where
    f (PLetter {letterSymbol = s}) =
      PLetter {letterSymbol = s, letterParams = []}

replacementWithContext :: (Production, Env) -> LWord Letter
replacementWithContext (p, env) =
  let LWord replacementWord = replacement p
   in LWord . map f $ replacementWord
  where
    f l =
      PLetter
        { letterSymbol = letterSymbol l
        , letterParams = map (eval env) (letterParams l)
        }

eval :: Env -> Expr -> Double
eval env expr =
  let y = eval' env expr
   in y

eval' _ (ExprConst d) = d
eval' env@(Env varMap) (ExprVar d) =
  eval env (M.findWithDefault (ExprConst 0) d varMap)
eval' env (ExprOp2 Sum a b) = eval env a + eval env b
eval' env (ExprOp2 Product a b) = eval env a * eval env b
eval' env (ExprOp2 Fraction a b) = eval env a / eval env b
eval' env (ExprOp2 Exponent a b) = eval env a ** eval env b

step ::
     StatefulGen g m => (LWord Letter) -> Productions -> g -> m (LWord Letter)
step (LWord axiom) productions gen = do
  let axiomWithContext =
        zip
          (zip axiom (extractPres axiom $ prodsIgnore productions))
          (extractPosts axiom $ prodsIgnore productions)
  parts <- mapM (matchProduction productions gen) axiomWithContext
  return $ foldl (<>) mempty $ map replacementWithContext parts

stepNM ::
     StatefulGen g m
  => Int
  -> LWord Letter
  -> Productions
  -> g
  -> m (LWord Letter)
stepNM 0 axiom _ _ = return axiom
stepNM n axiom rules gen = do
  word <- step axiom rules gen
  -- traceM . show $ word
  stepNM (n - 1) word rules gen

stepN :: RandomGen g => g -> Int -> LWord Letter -> Productions -> LWord Letter
stepN gen n axiom rules = fst $ runStateGen gen (stepNM n axiom rules)

runSystemDebug ::
     String -> Int -> Double -> String -> [(String, String)] -> IO ()
runSystemDebug name n theta axiom ps = do
  let gen = mkStdGen 42
  let LWord word = stepN gen n (lword axiom) (mkProductions ps)
  let debugWords =
        map LWord . drop 1 . map reverse . reverse . tails . reverse $ word
  let baseDir = "output/" <> name
  contentVar <- newIORef ""
  removeDirectoryRecursive baseDir
  createDirectory baseDir
  forM_ (zip debugWords [1 ..]) $ \(w, i) -> do
    let content =
          generateSvg
            (emptyPicture
               {pictureProjection = projectPathOrtho, pictureTheta = theta})
            w
    lastContent <- readIORef contentVar
    traceM $ (show i <> ": " <> show w)
    if content /= lastContent
      then writeFile (baseDir <> "/" <> show i <> ".svg") content
      else return ()
    writeIORef contentVar content

runSystem name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg") $
    generateSvg
      (emptyPicture {pictureProjection = projectPathOrtho, pictureTheta = theta}) $
    stepN gen n (lword axiom) (mkProductions ps)

runSystem2 name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg") $
    generateSvg
      (emptyPicture {pictureProjection = projectPathOrtho, pictureTheta = theta}) $
    stepN gen n (parseUnsafe axiom) ps

runPicture :: Picture -> IO ()
runPicture p = do
  let gen = mkStdGen (pictureSeed p)
  let word = stepN gen (pictureN p) (pictureAxiom p) (pictureProductions p)
  when (pictureDebug p) $ do
    putStrLn ""
    putStrLn ""
    putStrLn $ "axiom: " <> show word
  let svg = generateSvg p word
  let filename = "output/" <> (pictureTitle p) <> ".svg"
  writeFile filename svg

ignore = id

runSystem3D :: String -> Int -> Double -> String -> [(String, String)] -> IO ()
runSystem3D name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg") $
    generateSvg
      (emptyPicture {pictureProjection = projectPathIso, pictureTheta = theta}) $
    stepN gen n (parseUnsafe axiom) (mkProductions ps)

singleCharWord = intercalate " " . fmap pure

mkPicture name n theta axiom ps =
  emptyPicture
    { pictureTitle = name
    , pictureAxiom = parseUnsafe axiom
    , pictureN = n
    , pictureTheta = theta
    , pictureProductions = productions ps
    }

main2 = defaultMain tests

main = do
  runPicture $
    (mkPicture "aono-kunii-4" 10 1 ("A(1, 10)") [])
      { pictureStrokeWidth = 0.01
      , pictureDebug = False
      , pictureProjection = projectPathIso
      , pictureProductions =
          withDefines
            [ ("r1", "0.9")
            , ("r2", "0.8")
            , ("a1", "35")
            , ("a2", "35")
            , ("wr", "0.707")
            ] $
          productions
            [ ( match "A(l, w)"
              , "!(w) F(l) [ &(a1) B(l*r1, w*wr) ] /(180) [ &(a2) B(l*r2, w*wr) ]")
            , ( match "B(l, w)"
              , "!(w) F(l) [ +(a1) $ B(l*r1, w*wr) ] [ -(a2) $ B(l*r2, w*wr) ]")
            ]
      }
  -- Penrose tiling using the "classic" L-System method. This works well for
  -- stenciling the tiling, but is tricky to color since the tiles are all
  -- drawn interspersed with one another.
  runPicture $
    (mkPicture
       "penrose"
       4
       36
       (singleCharWord "[N]++[N]++[N]++[N]++[N]")
       [ (match "M", singleCharWord "OF++PF----NF[-OF----MF]++")
       , (match "N", singleCharWord "+OF--PF[---MF--NF]+")
       , (match "O", singleCharWord "-MF++NF[+++OF++PF]-")
       , (match "P", singleCharWord "--OF++++MF[+PF++++NF]--NF")
       , (match "F", "")
       ])
      { pictureViewport = ViewportFixed (V2 (-5) (-5), V2 5 5)
      , pictureStrokeWidth = 0.03
      }
  -- An alternate method based on the sub-divisions described at
  -- https://preshing.com/20110831/penrose-tiling-explained/
  --
  -- For ease of coloring and comprehension, the method is split into two
  -- "parts". The first part are the symbols {dL, dR, rL, rR} which map exactly
  -- to the two pairs of left/right triangles needed for the method, and which
  -- always produce a sub-division of themselves per instructions.
  --
  -- They always paired with turtle instruction to draw the whole shape (rather
  -- than just the half they are tracking), but these instructions produce
  -- nothing - in the other words they always "dissappear" and are replaced in
  -- the next step by fresh instructions. This does result in some doubling up
  -- along the edges, but this doesn't interfere with how they are currently
  -- rendered.
  --
  -- While not strictly necessary, for cleanliness {<, >} symbols are added
  -- such that all {+, -} symbols between them can be removed each step.
  -- (Without this they would simply hang around redundantly at the end of the
  -- word.) This isn't needed for {F} because that symbol is only ever used
  -- inside of {<, >}
  let diamondL = "dL(x * gr) < {(1) F(x) + F(x) + + + + F(x) + F(x) } >"
  let diamondR = "dR(x * gr) < {(1) F(x) - F(x) - - - - F(x) - F(x) } >"
  let rhombusL = "rL(x * gr) < {(2) F(x) - - F(x) - - - F(x) - - F(x) } >"
  let rhombusR = "rR(x * gr) < {(2) F(x) + + F(x) + + + F(x) + + F(x) } >"
  runPicture $
    (mkPicture
       "penrose-2"
       0
       36
       (intercalate " " . replicate 5 $ "[ dL(1) ] [ dR(1) ] + + ")
       [])
      { pictureStrokeWidth = 0.01
      , pictureDebug = False
      , pictureN = 5
      -- https://coolors.co/ef476f-ffd166-06d6a0-118ab2-073b4c
      , pictureColors = ["#000000", "#EF476F", "#FFD166"]
      , pictureViewport = ViewportBoundingRect 0.05
      , pictureProductions =
          withDefines
            [ ("gr", showFullPrecision (2 / (1 + sqrt (5)))) -- Golden Ratio
            ] $
          withIgnore "F + -" $
          productions
            [ (match "a(x)", rhombusL)
            -- For each of these four production rules: move the turtle to a
            -- new corner, then draw the required sub-divisions at a smaller
            -- scale.
            , ( match "dL(x)"
              , "+ f(x / gr) | + [ " <> rhombusL <> " ] " <> diamondL)
            , ( match "rL(x)"
              , "- f(x / gr / gr) | [ " <> rhombusL <> " ] f(x) - [ " <>
                diamondR <>
                " ] " <>
                rhombusR)
            , ( match "dR(x)"
              , "- f(x / gr) | - [ " <> rhombusR <> " ] " <> diamondR)
            , ( match "rR(x)"
              , "+ f(x / gr / gr) | [ " <> rhombusR <> " ] f(x) + [ " <>
                diamondL <>
                " ] " <>
                rhombusL)
            , (match "<", "")
            , (match ">", "")
            , (match "F", "")
            , ("<" <| match "+" |> ">", "")
            , ("<" <| match "-" |> ">", "")
            ]
      }
  let hondaConstants =
        [ (0.9, 0.6, 45, 45)
        , (0.9, 0.9, 45, 45)
        , (0.9, 0.8, 45, 45)
        , (0.9, 0.7, 30, -30)
        ]
  forM_ (zip hondaConstants [1 ..]) $ \((r1, r2, a0, a2), i) ->
    runPicture $
    (mkPicture ("honda-" <> show i) 10 1 "A(1, 10)" [])
      { pictureStrokeWidth = 0.01
      , pictureProjection = projectPathIso
      , pictureProductions =
          withDefines
            [ ("r1", showFullPrecision r1) -- contraction ratio for the trunk
            , ("r2", showFullPrecision r2) -- contraction ratio for branches
            , ("a0", showFullPrecision a0) -- branching angle from the trunk
            , ("a2", showFullPrecision a2) -- branching angle for lateral axes
            , ("d", "137.5") -- divergence angle
            , ("wr", "0.707") -- width decrease rate
            ] $
          productions
            [ ( match "A(l, w)"
              , "!(w) F(l) [ &(a0) B(l*r2,w*wr)   ] /(d) A(l*r1,w*wr)")
            , ( match "B(l, w)"
              , "!(w) F(l) [ -(a2) $ C(l*r2,w*wr) ]      C(l*r1, w*wr)")
            , ( match "C(l, w)"
              , "!(w) F(l) [ +(a2) $ B(l*r2,w*wr) ]      B(l*r1, w*wr)")
            ]
      }
  runPicture $
    emptyPicture
      { pictureTitle = "parametric-1"
      , pictureAxiom = parseUnsafe "F(1)"
      , pictureN = 6
      , pictureTheta = 86
      , pictureStrokeWidth = 0.002
      , pictureProductions =
          withDefines
            [("c", "1"), ("p", "0.3"), ("q", "c - p"), ("h", "(p * q) ^ 0.5")] $
          productions
            [(match "F(x)", "F(x * p) + F(x * h) - - F(x * h) + F(x * q)")]
      }
  runPicture $
    emptyPicture
      { pictureTitle = "parametric-2"
      , pictureAxiom = parseUnsafe "A(1)"
      , pictureN = 10
      , pictureTheta = 84
      , pictureStrokeWidth = 0.01
      , pictureProductions =
          withDefines [("R", "1.456")] $
          productions [(match "A(s)", "F(s) [ + A(s/R) ] [ - A(s/R) ]")]
      }
  runSystem2 "parametric-test" 0 36 "F [ + F(0.5) +(0.8) F(0.25) ] -(2) F" $
    productions []
  -- One suspicion for why these don't work is the angle alternation:
  --
  --    "According to the original interpretation, consecutive branches are
  --    issued alternately to the left and right, whereas turtle interpretation
  --    requires explicit specification of branching angles within the L-system
  --      - p33
  --
  -- For further investigation, a thought: if ignores were per rule, could
  -- achieve this with something like "-" < "-" => "+" (ignore NOT +-)
  runSystem2 "branching-7" 30 22.5 "F 1 F 1 F 1" $
    withIgnore "F + - [ ]" $
    productions
      [ ("0" <| match "0" |> "0", "0")
      , ("0" <| match "0" |> "1", "1 [ + F 1 F 1 ]")
      , ("0" <| match "1" |> "0", "1")
      , ("0" <| match "1" |> "1", "1")
      , ("1" <| match "0" |> "0", "0")
      , ("1" <| match "0" |> "1", "1 F 1")
      , ("1" <| match "1" |> "0", "0")
      , ("1" <| match "1" |> "1", "0")
      , (match "+", "-")
      , (match "-", "+")
      ]
  runSystem2 "branching-8" 30 22.5 "F 1 F 1 F 1" $
    withIgnore "F + - [ ]" $
    productions
      [ ("0" <| match "0" |> "0", "1")
      , ("0" <| match "0" |> "1", "1 [ - F 1 F 1 ]")
      , ("0" <| match "1" |> "0", "1")
      , ("0" <| match "1" |> "1", "1")
      , ("1" <| match "0" |> "0", "0")
      , ("1" <| match "0" |> "1", "1 F 1")
      , ("1" <| match "1" |> "0", "1")
      , ("1" <| match "1" |> "1", "0")
      , (match "+", "-")
      , (match "-", "+")
      ]
  runSystem
    "stochastic"
    5
    20
    "S"
    [ ("S", "S [ / / & & L ] [ / / ^ ^ L ] F S")
    , ("S", "S F S")
    , ("S", "S")
    , ("L", "[ ' F ]")
    ]
  runSystem3D
    "plant"
    4
    18
    "P"
    [ ("P", "I + [ P + Fl ] - - / / [ - - L ] I [ + + L ] - [ P Fl ] + + P Fl")
    , ("I", "F S [ / / & & L ] [ / / ^ ^ L ] F S")
    , ("S", "S F S")
    --, ("L", "[ ' { + F - F F - F + | + F - F F - F } ]")
    , ("L", "[ ' { + F - F - F + | + F - F - F } ]")
    , ("Fl", "[ & & & C ' / W / / / / W / / / / W / / / / W / / / / W ]")
    , ("C", "F F")
    , ("W", "[ ' ^ F ] [ { & & & & - F + F | - F + F } ]")
    ]
  runSystem
    "koch-island"
    3
    90.0
    "F - F - F - F"
    [("F", "F - F + F + F F - F - F + F")]
  runSystem3D
    "hilbert"
    3
    90
    "A"
    [ ("A", "B - F + C F C + F - D & F ^ D - F + & & C F C + F + B / /")
    , ("B", "A & F ^ C F B ^ F ^ D ^ ^ - F - D ^ | F ^ B | F C ^ F ^ A / /")
    , ("C", "| D ^ | F ^ B - F + C ^ F ^ A & & F A & F ^ C + F + B ^ F ^ D / /")
    , ("D", "| C F B - F + B | F A & F ^ A & & F B - F + B | F C / /")
    ]
  runSystem
    "islands-lakes"
    2
    90.0
    "F + F + F + F"
    [ ( "F"
      , "F + f - F F + F + F F + F f + F F - f + F F - F - F F - F f - F F F")
    ]
  runSystem "koch-tiles" 3 90.0 "F - F - F - F" [("F", "F F - F + F - F - F F")]
  runSystem "koch-spiral" 4 90.0 "F - F - F - F" [("F", "F - F + F - F - F")]
  runSystem
    "dragon-curve"
    9
    90.0
    "F◀"
    [("F◀", "F◀ + F▶ +"), ("F▶", "- F◀ - F▶")]
  runSystem
    "gosper-hex-curve"
    4
    60.0
    "FL"
    [ ("FL", "FL + FR + + FR - FL - - FL FL - FR +")
    , ("FR", "- FL + FR FR + + FR + FL - - FL - FR")
    ]
  runSystem "branching-1" 4 25.7 "F" [("F", "F [ + F ] F [ - F ] F")]
  runSystem "branching-2" 4 20 "F" [("F", "F [ + F ] F [ - F ] [ F ]")]
  runSystem
    "branching-3"
    4
    22.5
    "F"
    [("F", "F F - [ - F + F + F ] + [ + F - F - F ]")]
  runSystem
    "branching-4"
    7
    20
    "X"
    [("X", "F [ + X ] F [ - X ] + X"), ("F", "F F")]
  runSystem
    "branching-5"
    6
    25.7
    "X"
    [("X", "F [ + X ] [ - X ] F X"), ("F", "F F")]
  runSystem
    "branching-6"
    5
    22.5
    "X"
    [("X", "F - [ [ X ] + X ] + F [ + F X ] - X"), ("F", "F F")]

type Point = V3 Double

type ProjectedPoint = V2 Double

data Instruction a
  = MovePenDown a
  | MovePenUp a
  | ChangeColor Int
  | StrokeWidth Double
  | Fill Int
  deriving (Show)

defaultStrokeWidth = 0.1

defaultColors :: [String]
defaultColors =
  ["#50514F", "#109648", "#CB793A", "#ffff00", "#8D91C7", "#000000", "#ffff00"]

colors = defaultColors

extrude ::
     Double
  -> (ProjectedPoint, ProjectedPoint)
  -> (ProjectedPoint, ProjectedPoint)
extrude t (V2 x1 y1, V2 x2 y2) =
  let sx = (x2 - x1) * t
      sy = (y2 - y1) * t
   in (mkVec2 (x1 - sx) (y1 - sy), mkVec2 (x2 + sx) (y2 + sy))

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

toStyle :: Picture -> SvgPath -> String
toStyle picture path =
  intercalate ";" . map (\(a, b) -> a <> ":" <> b) $
  [ ("fill", maybe "none" toRGB $ pathFill path)
  , ("stroke", toRGB $ pathStroke path)
  , ( "stroke-width"
    , showFullPrecision (pathStrokeWidth path * (pictureStrokeWidth picture)) <>
      "px")
  ]
  where
    cs = pictureColors picture
    toRGB n = cs !! (n `mod` length cs)

generateSvg p (LWord ls) = renderSvg svgDoc
  where
    projectF = pictureProjection p
    theta = pictureTheta p
    strokeWidth = pictureStrokeWidth p
    thetaRads = theta / 180.0 * pi
    unprojected = toPath thetaRads ls
    tracer =
      if pictureDebug p
        then \x -> Debug.Trace.trace ("\nturtle: " <> show x) x
        else id
    is = projectF $ tracer unprojected
    --is = projectF unprojected
    svgDoc :: S.Svg
    svgDoc =
      let (V2 minX minY, V2 maxX maxY) =
            case pictureViewport p of
              ViewportBoundingRect n -> extrude n (bounds is)
              ViewportFixed x -> x
          aspect = (maxX - minX) / (maxY - minY)
       in S.docTypeSvg ! A.version "1.1" ! A.width (S.toValue $ 500 * aspect) !
          A.height "500" !
          A.viewbox
            (S.toValue . intercalate " " . map show $
             [minX, minY, maxX - minX, maxY - minY]) $ do
            S.g $ do
              S.rect ! A.x (S.toValue minX) ! A.y (S.toValue minY) !
                A.width (S.toValue $ maxX - minX) !
                A.height (S.toValue $ maxY - minY) !
                A.fill "#CBD4C2"
              forM_ (turtleToSvgPath is) $ \path -> do
                S.path !
                  A.style
                    (S.toValue $ "stroke-linecap:square;" <> toStyle p path) !
                  A.d
                    (mkPath $ do
                       let (V2 x y) = pathStart path
                       m x y
                       forM_ (pathPoints path) $ \(V2 x y) -> l x y)

data SvgPath = SvgPath
  { pathStart :: ProjectedPoint
  , pathPoints :: [ProjectedPoint]
  , pathStroke :: Int
  , pathStrokeWidth :: Double
  , pathFill :: Maybe Int
  } deriving (Show, Eq)

mkSvgPath start =
  SvgPath
    { pathStart = start
    , pathPoints = mempty
    , pathStroke = 0
    , pathStrokeWidth = 1
    , pathFill = Nothing
    }

turtleToSvgPath :: [Instruction ProjectedPoint] -> [SvgPath]
turtleToSvgPath is = snd $ execRWST f () (mkSvgPath (V2 0 0)) []
  where
    f = do
      forM_ (is <> [MovePenUp (V2 0 0)]) $ \i -> do
        pathUnderConstruction <- get
        let pathStarted = not . null . pathPoints $ pathUnderConstruction
        case i of
          MovePenDown p -> do
            put $
              pathUnderConstruction
                {pathPoints = pathPoints pathUnderConstruction <> [p]}
          _ -> do
            when pathStarted $ do
              tell [pathUnderConstruction]
              -- TODO: Handle when path has no points
              put
                (pathUnderConstruction
                   { pathStart =
                       head . reverse . pathPoints $ pathUnderConstruction
                   , pathPoints = mempty
                   })
            case i of
              MovePenUp x -> modify (\p -> p {pathStart = x})
              ChangeColor x -> modify (\p -> p {pathStroke = x})
              StrokeWidth x -> modify (\p -> p {pathStrokeWidth = x})
              Fill x -> modify (\p -> p {pathFill = Just x})

bounds is =
  let (mn, mx) =
        foldl
          (\((V2 minX minY), (V2 maxX maxY)) pos ->
             let (V2 x y) = pos
              in ( mkVec2 (min minX x) (min minY y)
                 , mkVec2 (max maxX x) (max maxY y)))
          (mkVec2 0 0, mkVec2 0 0)
          (map toCoords is)
   in (mn, mx)

projectPathIso :: [Instruction Point] -> [Instruction ProjectedPoint]
projectPathIso = map f
  where
    f (MovePenDown x) = MovePenDown $ p x
    f (MovePenUp x) = MovePenUp $ p x
    f (ChangeColor x) = ChangeColor x
    f (StrokeWidth x) = StrokeWidth x
    f (Fill x) = Fill x
    p v3 =
      let (V3 x y _) = (isoProjection !* v3)
       in V2 x y

projectPathOrtho :: [Instruction Point] -> [Instruction ProjectedPoint]
projectPathOrtho = map f
  where
    f (MovePenDown x) = MovePenDown $ p x
    f (MovePenUp x) = MovePenUp $ p x
    f (ChangeColor x) = ChangeColor x
    f (StrokeWidth x) = StrokeWidth x
    f (Fill x) = Fill x
    p v3 =
      let (V3 x y _) = orthoProjection !* v3
       in V2 x y

toCoords (MovePenDown c) = c
toCoords (MovePenUp c) = c
toCoords _ = (mkVec2 0 0)

mkVec2 = V2

mkVec3 = V3

zeroV = V3 0 0 0

rotateU a = V3 (V3 (cos a) (sin a) 0) (V3 ((-1) * (sin a)) (cos a) 0) (V3 0 0 1)

rotateL a = V3 (V3 (cos a) 0 (sin a * (-1))) (V3 0 1 0) (V3 (sin a) 0 (cos a))

rotateH a = V3 (V3 1 0 0) (V3 0 (cos a) (sin a * (-1))) (V3 0 (sin a) (cos a))

data TurtleState = TurtleState
  { rotateM :: V3 Point
  , position :: Point
  , color :: Int
  , turtleStrokeWidth :: Double
  , turtleFill :: Maybe Int
  } deriving (Show)

initialTurtle =
  TurtleState
  -- We're making trees here, most of the time we want them to grow up, so
  -- let's start by pointing directly along the Y-axis.
    { rotateM =
        (rotateU $ pi / 2.0) !*!
        V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
    , position = V3 0 0 0
    , color = 0
    , turtleStrokeWidth = 1
    , turtleFill = Nothing
    }

toPath :: Double -> [Letter] -> [Instruction Point]
toPath thetaRads ls =
  concat . catMaybes $ evalState (mapM fInit ls) [initialTurtle]
  where
    fInit :: Letter -> State [TurtleState] (Maybe [Instruction Point])
    fInit l = f (letterSymbol l, head $ letterParams l <> [1.0])
    f :: (String, Double) -> State [TurtleState] (Maybe [Instruction Point])
    f (('F':_), a) = do
      (V3 h _ _) <- gets (rotateM . head)
      modifyP (h ^* a)
      v <- gets (position . head)
      return . Just $ [MovePenDown v]
    f (('f':_), a) = do
      (V3 h _ _) <- gets (rotateM . head)
      modifyP (h ^* a)
      v <- gets (position . head)
      return . Just $ [MovePenUp v]
    f ("[", _) = do
      modify (\(x:xs) -> (x : x : xs))
      return Nothing
    f ("]", _) = do
      modify (\(x:xs) -> xs)
      v2 <- gets (position . head)
      c <- gets (color . head)
      return . Just $ [MovePenUp v2, ChangeColor c]
    f ("+", a) = do
      modifyR (rotateU $ thetaRads * a)
      return Nothing
    f ("-", a) = do
      modifyR (rotateU $ (-thetaRads) * a)
      return Nothing
    f ("&", a) = do
      modifyR (rotateL $ thetaRads * a)
      return Nothing
    f ("^", a) = do
      modifyR (rotateL $ (-thetaRads) * a)
      return Nothing
    f ("\\", a) = do
      modifyR (rotateH $ thetaRads * a)
      return Nothing
    f ("/", a) = do
      modifyR (rotateH $ (-thetaRads) * a)
      return Nothing
    f ("|", _) = do
      modifyR (rotateU pi)
      return Nothing
    f ("'", _) = do
      modifyC (+ 1)
      c <- gets (color . head)
      return . Just $ [ChangeColor c]
    f ("!", a) = do
      modifyStroke (const a)
      return . Just $ [StrokeWidth a]
    f ("{", a) = do
      modifyFill (const . Just . round $ a)
      return . Just $ [Fill . round $ a]
    f ("}", _) = do
      modifyFill (const Nothing)
      return . Just $ [Fill Nothing]
    f ("$", _) = do
      currentR <- gets (rotateM . head)
      let newRotateM = turtleDollar currentR
      modify (\(t:ts) -> (t {rotateM = newRotateM} : ts))
      return Nothing
    f _ = return Nothing
    -- f unknown = error $ "unimplemented: " <> show unknown

turtleDollar :: V3 Point -> V3 Point
turtleDollar (V3 h l u) =
  let v = V3 0.0 1.0 0.0
      l' = (cross v h) ^/ (norm $ cross v h)
      u' = cross h l'
   in (V3 h l' u')

modifyP m = modify (\(t:ts) -> (t {position = position t + m} : ts))

modifyR m = modify (\(t:ts) -> (t {rotateM = m !*! (rotateM t)} : ts))

modifyC m = modify (\(t:ts) -> (t {color = m (color t)} : ts))

modifyStroke m =
  modify (\(t:ts) -> (t {turtleStrokeWidth = m (turtleStrokeWidth t)} : ts))

modifyFill m = modify (\(t:ts) -> (t {turtleFill = m (turtleFill t)} : ts))

mkProductions :: [(String, String)] -> Productions
mkProductions template =
  emptyProductions
    { prodsRules =
        map
          (\(l, w) ->
             Production
               {prodRule = mkRule l, replacement = parseUnsafeWordExpr w})
          template
    }

mkProductions2 :: [(MatchRule, String)] -> Productions
mkProductions2 template =
  emptyProductions
    { prodsRules =
        map
          (\(r, w) ->
             Production {prodRule = r, replacement = parseUnsafeWordExpr w})
          template
    }

withIgnore :: String -> Productions -> Productions
withIgnore list ps = ps {prodsIgnore = map letter $ words list}

withDefines list ps =
  ps {prodsDefines = Env . M.map parseUnsafeExpr . M.fromList $ list}

productions = mkProductions2

tests =
  let gen = mkStdGen 42
   in testGroup
        "L-Systems"
        [ testGroup
            "Deterministic & Context-free (DOL)"
            [ testCase "Trivial" $
              (lword "a b a b a a b a") @=?
              (stepN gen 5 (lword "b") $
               mkProductions [("b", "a"), ("a", "b a")])
            , testCase "Anabaena catenula" $
              (lword "b◀ a▶ b◀ a▶ a▶ b◀ a▶ a▶") @=?
              (stepN gen 4 (lword "a▶") $
               mkProductions
                 [("a▶", "a◀ b▶"), ("a◀", "b◀ a▶"), ("b▶", "a▶"), ("b◀", "a◀")])
            , testCase "Koch island" $
              (lword
                 "F - F + F + F F - F - F + F - F - F + F + F F - F - F + F - F - F + F + F F - F - F + F - F - F + F + F F - F - F + F") @=?
              (stepN gen 1 (lword "F - F - F - F") $
               mkProductions [("F", "F - F + F + F F - F - F + F")])
            , testCase "Bracketed" $
              (lword "F [ + F ] F [ - F ] F") @=?
              (stepN gen 1 (lword "F") $
               mkProductions [("F", "F [ + F ] F [ - F ] F")])
            ]
        , testGroup
            "Context-sensitive"
            [ testCase "Trivial pre-condition" $
              (lword "a a b") @=?
              (stepN gen 2 (lword "b a a") $
               productions [("b" <| match "a", "b"), (mkRule "b", "a")])
            , testCase "Trivial post-condition" $
              (lword "b a a") @=?
              (stepN gen 2 (lword "a a b") $
               productions [(match "a" |> "b", "b"), (match "b", "a")])
            , testCase "Trivial ignore" $
              (lword "b a + - a") @=?
              (stepN gen 2 (lword "a a + - b") $
               withIgnore "+ -" $
               productions [(match "a" |> "b", "b"), (match "b", "a")])
            , testCase "Parametric ignore" $
              (parseUnsafe "+ F F(1)") @=?
              (stepN gen 2 (parseUnsafe "+ F < + F(1) - + >") $
               withIgnore "+ - F" $
               productions
                 [ ("<" <| match "+" |> ">", "")
                 , ("<" <| match "-" |> ">", "")
                 , (match "<", "")
                 , (match ">", "")
                 ])
            ]
        , testGroup
            "Turtle to SVG"
            [ testCase "Basic path" $
              [(mkSvgPath (V2 0 0)) {pathPoints = [V2 1 1]}] @=?
              turtleToSvgPath [MovePenDown (V2 1 1)]
            , testCase "Setting meta-data" $
              [ (mkSvgPath (V2 0 0))
                  { pathPoints = [V2 1 1]
                  , pathStroke = 1
                  , pathStrokeWidth = 0.5
                  , pathFill = Just 2
                  }
              ] @=?
              turtleToSvgPath
                [ChangeColor 1, Fill 2, StrokeWidth 0.5, MovePenDown (V2 1 1)]
            , testCase "Starts a new path when metadata changes" $
              [ (mkSvgPath (V2 0 0)) {pathPoints = [V2 1 1], pathStroke = 1}
              , (mkSvgPath (V2 1 1))
                  {pathPoints = [V2 2 2], pathStroke = 1, pathFill = Just 2}
              ] @=?
              turtleToSvgPath
                [ ChangeColor 1
                , MovePenDown (V2 1 1)
                , Fill 2
                , MovePenDown (V2 2 2)
                ]
            , testCase "Starts a new path when pen up" $
              [ (mkSvgPath (V2 0 0)) {pathPoints = [V2 1 1], pathStroke = 2}
              , (mkSvgPath (V2 0 0)) {pathPoints = [V2 2 2], pathStroke = 2}
              ] @=?
              turtleToSvgPath
                [ ChangeColor 2
                , MovePenDown (V2 1 1)
                , MovePenUp (V2 0 0)
                , MovePenDown (V2 2 2)
                ]
            ]
        , testGroup
            "Parametric"
            [ testCase "Basic substitution" $
              (parseUnsafe "G(1, 2)") @=?
              (stepN gen 1 (parseUnsafe "F(1, 2)") $
               productions [(match "F(x, y)", "G(x,y)")])
            , testCase "Addition" $
              (parseUnsafe "F(3)") @=?
              (stepN gen 2 (parseUnsafe "F(1)") $
               productions [(match "F(x)", "F(x+1)")])
            , testCase "Arithmetic" $
              (parseUnsafe "F(4)") @=?
              (stepN gen 1 (parseUnsafe "F(1)") $
               productions [(match "F(x)", "F((x*(x+9)/5)^2)")])
            , testCase "Defines" $
              (parseUnsafe "F(4)") @=?
              (stepN gen 2 (parseUnsafe "F(1)") $
               withDefines [("y", "x*2")] $ productions [(match "F(x)", "F(y)")])
            , testCase "Pre-conditions" $
              (parseUnsafe "a a b(1)") @=?
              (stepN gen 2 (parseUnsafe "b(1) a a") $
               productions [("b(x)" <| match "a", "b(x)"), (match "b", "a")])
            , testCase "Post-conditions" $
              (parseUnsafe "b(2) a a") @=?
              (stepN gen 2 (parseUnsafe "a a b(2)") $
               productions [(match "a" |> "b(x)", "b(x)"), (match "b", "a")])
            , testCase "Guards" $
              (parseUnsafe "b a(2) c(3) d(4)") @=?
              (stepN gen 1 (parseUnsafe "a(1) a(2) a(3) a(4)") $
               productions
                 [ (match "a(x)" |: "x < 2", "b")
                 , (match "a(x)" |: "x = 3", "c(x)")
                 , (match "a(x)" |: "x >= 4", "d(x)")
                 ])
            , testCase "Guards with defines" $
              (parseUnsafe "b a(2) c(3)") @=?
              (stepN gen 1 (parseUnsafe "a(1) a(2) a(3)") $
               withDefines [("y", "3")] $
               productions
                 [ (match "a(x)" |: "x <= 1", "b")
                 , (match "a(x)" |: "x + y > 5", "c(x)")
                 ])
            , testCase "Guard takes precedence" $
              (parseUnsafe "b c(2)") @=?
              (stepN gen 1 (parseUnsafe "a(1) a(2)") $
               productions
                 [(match "a(x)", "c(x)"), (match "a(x)" |: "x <= 1", "b")])
            , testGroup
                "Parsing"
                [ testCase "One param" $
                  LWord [mkPLetter "F" [3]] @=? (parseUnsafe "F(3)")
                , testCase "Decimal" $
                  LWord [mkPLetter "F" [3.55]] @=? (parseUnsafe "F(3.55)")
                , testCase "No param" $
                  LWord [mkPLetter "F" []] @=? (parseUnsafe "F")
                , testCase "No param brackets" $
                  LWord [mkPLetter "F" []] @=? (parseUnsafe "F()")
                , testCase "Two param" $
                  LWord [mkPLetter "F" [3, 4]] @=? (parseUnsafe "F(3,4)")
                , testCase "Two param whitespace" $
                  LWord [mkPLetter "F" [3, 4]] @=? (parseUnsafe "F( 3 , 4  )")
                , testCase "Two var" $
                  LWord [mkPLetter "F" [], mkPLetter "G" [2]] @=?
                  (parseUnsafe "F G(2)")
                , testCase "Whitespace" $
                  LWord [mkPLetter "F" []] @=? (parseUnsafe "  F  ")
                , testCase "Expressions" $
                  LWord [mkPLetter "F" [], mkPLetter "+" []] @=?
                  (parseUnsafeWordExpr "F +")
                , testCase "Addition" $
                  LWord
                    [mkPLetter "F" [ExprOp2 Sum (ExprVar "x") (ExprConst 1)]] @=?
                  (parseUnsafeWordExpr "F(x+1)")
                , testCase "Subtraction" $
                  LWord
                    [ mkPLetter
                        "F"
                        [ ExprOp2
                            Sum
                            (ExprVar "x")
                            (ExprOp2 Product (ExprConst 1) (ExprConst (-1)))
                        ]
                    ] @=?
                  (parseUnsafeWordExpr "F(x-1)")
                , testCase "Multiplication" $
                  LWord
                    [ mkPLetter
                        "F"
                        [ExprOp2 Product (ExprVar "x") (ExprConst 1)]
                    ] @=?
                  (parseUnsafeWordExpr "F(x*1)")
                , testCase "Division" $
                  LWord
                    [ mkPLetter
                        "F"
                        [ExprOp2 Fraction (ExprVar "x") (ExprConst 1)]
                    ] @=?
                  (parseUnsafeWordExpr "F(x/1)")
                , testCase "Exponent" $
                  LWord
                    [ mkPLetter
                        "F"
                        [ExprOp2 Exponent (ExprVar "x") (ExprConst 1)]
                    ] @=?
                  (parseUnsafeWordExpr "F(x^1)")
                , testCase "Parens" $
                  LWord
                    [mkPLetter "F" [ExprOp2 Sum (ExprVar "x") (ExprVar "x")]] @=?
                  (parseUnsafeWordExpr "F(((x+x)))")
                , testCase "Patterns" $
                  mkPLetter "F" ["x", "y"] @=? (parseUnsafePattern "F(x, y)")
                ]
            ]
        ]

mkPLetter s p = PLetter {letterSymbol = s, letterParams = p}

makePath :: S.AttributeValue
makePath =
  mkPath $ do
    m 0 0
    l 2 3
    l 4 5

makeTransform :: S.AttributeValue
makeTransform = rotate 50
