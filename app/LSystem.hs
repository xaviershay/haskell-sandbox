{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import System.Directory (createDirectory, removeDirectoryRecursive)
import Debug.Trace
import Data.Maybe (catMaybes)
import Control.Monad (forM_, foldM, guard)
import Control.Monad.State (State(..), runState, modify, evalState, get, gets)
import Data.List (intercalate, tails)
import Data.Monoid ((<>))
import System.Random
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.HUnit
import Data.IORef
import qualified Text.Parsec as P
import Text.Parsec.Expr
import qualified Text.Parsec.Token    as Tok
import qualified Text.Parsec.Language as Tok
import qualified Data.HashMap.Strict as M


import Linear.V2
import Linear.V3
import Linear.Projection
import Linear.Matrix

import Text.Blaze.Svg11 ((!), mkPath, rotate, lr, mr, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
--import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = Tok.emptyDef
      { Tok.reservedOpNames = ["+", "^", "/", "*"]
      , Tok.reservedNames   = []
      , Tok.identStart      = P.letter
      }

reservedOp = Tok.reservedOp lexer
whiteSpace = Tok.whiteSpace lexer

parens = P.between (P.char '(' <* whiteSpace) (P.char ')')
termExpr = (parens exprParser
             <|> ExprConst <$> paramExpr
             <|> ExprVar <$> P.many1 exprSymbolExpr
           ) <* whiteSpace
--
--table = [ [postfix "!" (Op1 Factorial), series "S", limit "lim", functionExpr, prefix "-" (Op1 Negate) ]
--        , [binary "^" (Op2 Exponent) AssocLeft ]
--        , [binary "*" (Op2 Product) AssocLeft, binary "/" (Op2 Fraction) AssocLeft, binary "" (Op2 Product) AssocLeft]
--        , [binary "+" (Op2 Sum) AssocLeft, binary "-" (\a b -> Op2 Sum a (Op1 Negate b)) AssocLeft ]
--        ]

binary name fun assoc = Infix (do { reservedOp name; return fun}) assoc

table = [ [binary "^" (ExprOp2 Exponent) AssocLeft ]
        , [binary "*" (ExprOp2 Product) AssocLeft, binary "/" (ExprOp2 Fraction) AssocLeft]
        , [ binary "+" (ExprOp2 Sum) AssocLeft
          , binary "-" (\a b -> ExprOp2 Sum a (ExprOp2 Product b (ExprConst (-1)))) AssocLeft
          ]
        ]

exprParser = buildExpressionParser table (whiteSpace *> termExpr)

exprSymbolExpr = P.satisfy (\x -> not $ x `elem` (" (),+-*/^" :: String))
symbolExpr = P.satisfy (\x -> not $ x `elem` (" ()," :: String))

paramExpr = do
  leading <- P.many1 P.digit
  decimal <- P.optionMaybe (P.char '.' >> P.many1 P.digit)

  let trailing = case decimal of
                   Just x -> "." <> x
                   Nothing -> ""

  return $ read (leading <> trailing)

patternExpr = P.many1 symbolExpr

letterExpr paramParser = do
  symbol <- P.many1 symbolExpr
  params <- P.try (P.char '(' >> whiteSpace) *> (paramParser <* whiteSpace) `P.sepBy` (P.char ',' >> whiteSpace) <* (P.char ')') <|> pure []

  return $ PLetter {
    letterSymbol = symbol,
    letterParams = params
  }

pwordExpr = LWord <$> (whiteSpace *> P.many1 (letterExpr paramExpr <* whiteSpace))

pwordExprExpr = LWord <$> (whiteSpace *> P.many (letterExpr exprParser <* whiteSpace))

pwordPatternExpr = whiteSpace *> letterExpr patternExpr <* whiteSpace

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

isoProjection = (1 / sqrt 6) * V3
  (V3 (sqrt 3) 0 ((-1) * sqrt 3))
  (V3 1 2 1)
  (V3 (sqrt 2) ((-1) * sqrt 2) (sqrt 2))

orthoProjection = V3
  (V3 1.0 0 0)
  (V3 0.0 1 0)
  (V3 0.0 0 0)

data PLetter a = PLetter {
  letterSymbol :: String,
  letterParams :: [a]
} deriving (Eq)

plword s = parseUnsafe s

data Term2 = Sum | Product | Fraction | Exponent
  deriving (Show, Eq)

data Env = Env (M.HashMap String Expr)
  deriving (Show)

instance Semigroup Env where
  Env a <> Env b = Env (a <> b)

instance Monoid Env where
  mempty = Env mempty

data Expr =
  ExprConst Double
  | ExprVar String
  | ExprOp2 Term2 Expr Expr
  deriving (Show, Eq)

type LetterExpr = PLetter Expr
type Letter = PLetter Double
type LetterPattern = PLetter String

letter :: String -> Letter
letter a = PLetter { letterSymbol = a, letterParams = mempty }

instance Show a => Show (PLetter a) where
  show (PLetter { letterSymbol = s, letterParams = ps }) = s <> if null ps then "" else show ps

data LWord a = LWord [a]
  deriving (Eq)

instance Semigroup (LWord x) where
  (LWord a) <> (LWord b) = LWord $ a <> b
instance Monoid (LWord a) where
  mempty = LWord []

instance Show a => Show (LWord a) where
  show (LWord l) = intercalate " " $ map show l

data Production = Production {
  prodRule :: MatchRule,
  replacement :: LWord (LetterExpr)
} deriving (Show)

data MatchRule = MatchRule {
  ruleLetter :: LetterPattern,
  ruleLetterPre :: Maybe LetterPattern,
  ruleLetterPost :: Maybe LetterPattern
}

instance Show MatchRule where
  show rule = show (ruleLetter rule) <> " (" <> show (ruleLetterPre rule) <> ", " <> show (ruleLetterPost rule) <> ")"

match = mkRule
mkRule l = MatchRule
  { ruleLetter = parseUnsafePattern l
  , ruleLetterPre = Nothing
  , ruleLetterPost = Nothing
  }

applyRule :: MatchRule -> LetterContext -> Bool
applyRule r ((l, pre), post) =
  letterSymbol (ruleLetter r) == letterSymbol l
  && case ruleLetterPre r of
       Nothing -> True
       Just _ -> fmap letterSymbol (ruleLetterPre r) == fmap letterSymbol pre
  && case ruleLetterPost r of
       Nothing -> True
       Just _ -> fmap letterSymbol (ruleLetterPost r) == fmap letterSymbol post

l <| r = r { ruleLetterPre = Just (parseUnsafePattern l) }
r |> l = r { ruleLetterPost = Just (parseUnsafePattern l) }

lword :: String -> LWord Letter
lword = LWord . map letter . words

type LetterContext = ((Letter, Maybe Letter), Maybe Letter)

-- TODO: Fix for params
identityProduction :: LetterContext -> Production
identityProduction ((l, _), _) = Production { prodRule = mkRule (letterSymbol l), replacement = LWord [PLetter { letterSymbol = letterSymbol l, letterParams = []}] }

envFromContext :: Production -> LetterContext -> Env
envFromContext p ((l1, l2), l3) =
  let
    rule = prodRule p
    paramLabels = letterParams . ruleLetter $ rule
    paramValues = map ExprConst . letterParams $ l1
    xs = zip paramLabels paramValues
  in
  Env $ M.fromList xs

emptyEnv = Env mempty

matchProduction :: StatefulGen g m => Productions -> g -> LetterContext -> m (Production, Env)
matchProduction ps gen context  =
  let buildEnv x = prodsDefines ps <> envFromContext x context in
  case filter (\p -> prodRule p `applyRule` context) (prodsRules ps) of
    [x] -> return $ (x, buildEnv x)
    []  -> return $ (identityProduction context, emptyEnv)
    xs  -> do
      i <- uniformRM (0, length xs - 1) gen
      return $ let x = xs !! i in (x, buildEnv x)

data Productions = Productions {
  prodsRules :: [Production],
  prodsIgnore :: [Letter],
  prodsDefines :: Env
}

emptyProductions = Productions {
  prodsRules = mempty,
  prodsIgnore = mempty,
  prodsDefines = mempty
}

headMaybe (x:_) = Just x
headMaybe [] = Nothing

extractPosts word ignores =
  let f = filter (\x -> not $ x `elem` ignores) in
  map (headMaybe . f) . drop 1 . tails $ word

extractPres word = reverse . extractPosts (reverse word)

evalExpr :: LWord LetterExpr -> LWord Letter
evalExpr (LWord ls) = LWord . map f $ ls
  where
    f (PLetter { letterSymbol = s }) = PLetter { letterSymbol = s, letterParams = [] }

replacementWithContext :: (Production, Env) -> LWord Letter
replacementWithContext (p, env) =
  let
    LWord replacementWord = replacement p
  in
  LWord . map f $ replacementWord
  where
    f l = PLetter {
      letterSymbol = letterSymbol l,
      letterParams = map (eval env) (letterParams l)
    }

eval :: Env -> Expr -> Double
eval env expr = eval' env expr

eval' _ (ExprConst d) = d
eval' env@(Env varMap) (ExprVar d) = eval env (M.findWithDefault (ExprConst 0) d varMap)
eval' env (ExprOp2 Sum a b) = eval env a + eval env b
eval' env (ExprOp2 Product a b) = eval env a * eval env b
eval' env (ExprOp2 Fraction a b) = eval env a / eval env b
eval' env (ExprOp2 Exponent a b) = eval env a ** eval env b

step :: StatefulGen g m => (LWord Letter) -> Productions -> g -> m (LWord Letter)
step (LWord axiom) productions gen = do
  let axiomWithContext = zip
        (zip axiom (extractPres axiom $ prodsIgnore productions))
        (extractPosts axiom $ prodsIgnore productions)
  parts <- mapM (matchProduction productions gen) axiomWithContext
  return $ foldl (<>) mempty $ map replacementWithContext parts

stepNM :: StatefulGen g m => Int -> LWord Letter -> Productions -> g -> m (LWord Letter)
stepNM 0 axiom _ _ = return axiom
stepNM n axiom rules gen = do
  word <- step axiom rules gen

  -- traceM . show $ word
  stepNM (n - 1) word rules gen

stepN :: RandomGen g => g -> Int -> LWord Letter -> Productions -> LWord Letter
stepN gen n axiom rules = fst $ runStateGen gen (stepNM n axiom rules)

runSystemDebug :: String -> Int -> Double -> String -> [(String, String)] -> IO ()
runSystemDebug name n theta axiom ps = do
  let gen = mkStdGen 42
  let LWord word = stepN gen n (lword axiom) (mkProductions ps)
  let debugWords = map LWord . drop 1 . map reverse . reverse . tails . reverse $ word
  let baseDir = "output/" <> name

  contentVar <- newIORef ""

  removeDirectoryRecursive baseDir
  createDirectory baseDir
  forM_ (zip debugWords [1..]) $ \(w, i) -> do
    let content = generateSvg projectPathOrtho theta w
    lastContent <- readIORef contentVar

    traceM $ (show i <> ": " <> show w)
    if content /= lastContent then
      writeFile (baseDir <> "/" <> show i <> ".svg") content
    else
      return ()

    writeIORef contentVar content

runSystem name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg")
    $ generateSvg projectPathOrtho theta
    $ stepN gen n (lword axiom) (mkProductions ps)

runSystem2 name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg")
    $ generateSvg projectPathOrtho theta
    $ stepN gen n (parseUnsafe axiom) ps

ignore = id

runSystem3D :: String -> Int -> Double -> String -> [(String, String)] -> IO ()
runSystem3D name n theta axiom ps = do
  let gen = mkStdGen 42
  writeFile ("output/" <> name <> ".svg")
    $ generateSvg projectPathIso theta
    $ stepN gen n (parseUnsafe axiom) (mkProductions ps)

singleCharWord = intercalate " " . fmap pure

main = defaultMain tests
main2 = do
  runSystem "penrose" 4 36 (singleCharWord "[N]++[N]++[N]++[N]++[N]")
    [ ("M", singleCharWord "OF++PF----NF[-OF----MF]++")
    , ("N", singleCharWord "+OF--PF[---MF--NF]+")
    , ("O", singleCharWord "-MF++NF[+++OF++PF]-")
    , ("P", singleCharWord "--OF++++MF[+PF++++NF]--NF")
    , ("F", "")
    ]

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
  runSystem2 "branching-7" 30 22.5 "F 1 F 1 F 1"
    $ withIgnore "F + - [ ]"
    $ productions
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
  runSystem2 "branching-8" 30 22.5 "F 1 F 1 F 1"
    $ withIgnore "F + - [ ]"
    $ productions
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
  runSystem "stochastic" 5 20 "S"
    [ ("S", "S [ / / & & L ] [ / / ^ ^ L ] F S")
    , ("S", "S F S")
    , ("S", "S")
    , ("L", "[ ' F ]")
    ]

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

toPath :: Double -> [Letter] -> [Instruction Point]
toPath thetaRads ls = concat . catMaybes $ evalState (mapM fInit ls) [initialTurtle]
  where
    fInit :: Letter -> State [TurtleState] (Maybe [Instruction Point])
    fInit l = f (letterSymbol l, head $ letterParams l <> [1.0])

    f :: (String, Double) -> State [TurtleState] (Maybe [Instruction Point])
    f (('F':_), a) = do
      rv <- gets (rotateM . head)
      let dv = rv !* (mkVec3 a 0 0)
      modifyP dv
      v <- gets (position . head)
      return . Just $ [MovePenDown v]
    f ("f", a) = do
      rv <- gets (rotateM . head)
      let dv = rv !* (mkVec3 a 0 0)
      modifyP dv
      v <- gets (position . head)
      return . Just $ [MovePenUp v]
    f ("[", _) = do
      modify (\(x:xs) -> (x:x:xs))
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
    f _ = return Nothing
    -- f unknown = error $ "unimplemented: " <> show unknown

modifyP m = modify (\(t:ts) -> (t { position = position t + m }:ts))
modifyR m = modify (\(t:ts) -> (t { rotateM = rotateM t !*! m }:ts))
modifyC m = modify (\(t:ts) -> (t { color = m (color t) }:ts))

mkProductions :: [(String, String)] -> Productions
mkProductions template = emptyProductions {
  prodsRules = map (\(l, w) -> Production {
            prodRule = mkRule l,
            replacement = parseUnsafeWordExpr w
          }) template
}

mkProductions2 :: [(MatchRule, String)] -> Productions
mkProductions2 template = emptyProductions {
  prodsRules = map (\(r, w) -> Production {
            prodRule = r,
            replacement = parseUnsafeWordExpr w
          }) template
}

withIgnore :: String -> Productions -> Productions

withIgnore list ps = ps { prodsIgnore = map letter $ words list }
withDefines list ps = ps { prodsDefines = Env . M.map parseUnsafeExpr . M.fromList $ list }
productions = mkProductions2

tests = let gen = mkStdGen 42 in testGroup "L-Systems"
  [ testGroup "Deterministic & Context-free (DOL)"
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
  , testGroup "Context-sensitive"
    [ testCase "Trivial pre-condition"
      $ (lword "a a b") @=?
      (stepN gen 2 (lword "b a a")
      $ productions
          [
           ("b" <| match "a", "b")
          , (mkRule "b", "a")
          ]
          )
    , testCase "Trivial post-condition"
      $ (lword "b a a") @=?
      (stepN gen 2 (lword "a a b")
      $ productions
          [ (match "a" |> "b", "b")
          , (match "b", "a")
          ]
          )
    , testCase "Trivial ignore"
      $ (lword "b a + - a") @=?
      (stepN gen 2 (lword "a a + - b")
      $ withIgnore "+ -" $ productions
          [ (match "a" |> "b", "b")
          , (match "b", "a")
          ]
          )
    ]
  , testGroup "Parametric"
    [ testCase "Basic substitution"
      $ (parseUnsafe "G(1, 2)") @=?
      (stepN gen 1 (parseUnsafe "F(1, 2)") $ productions
        [ (match "F(x, y)", "G(x,y)")
        ])
    , testCase "Addition"
      $ (parseUnsafe "F(3)") @=?
      (stepN gen 2 (parseUnsafe "F(1)") $ productions
        [ (match "F(x)", "F(x+1)")
        ])
    , testCase "Arithmetic"
      $ (parseUnsafe "F(4)") @=?
      (stepN gen 1 (parseUnsafe "F(1)") $ productions
        [ (match "F(x)", "F((x*(x+9)/5)^2)")
        ])
    , testCase "Defines"
      $ (parseUnsafe "F(4)") @=?
      (stepN gen 2 (parseUnsafe "F(1)") $ withDefines [("y", "x*2")] $ productions
        [ (match "F(x)", "F(y)")
        ])
    , testGroup "Parsing"
      [ testCase "One param"
          $ LWord [mkPLetter "F" [3]] @=? (parseUnsafe "F(3)")
      , testCase "Decimal"
          $ LWord [mkPLetter "F" [3.55]] @=? (parseUnsafe "F(3.55)")
      , testCase "No param"
          $ LWord [mkPLetter "F" []]  @=? (parseUnsafe "F")
      , testCase "No param brackets"
          $ LWord [mkPLetter "F" []] @=? (parseUnsafe "F()")
      , testCase "Two param"
          $ LWord [mkPLetter "F" [3, 4]] @=? (parseUnsafe "F(3,4)")
      , testCase "Two param whitespace"
          $ LWord [mkPLetter "F" [3, 4]] @=? (parseUnsafe "F( 3 , 4  )")
      , testCase "Two var"
          $ LWord [mkPLetter "F" [], mkPLetter "G" [2]] @=? (parseUnsafe "F G(2)")
      , testCase "Whitespace"
          $ LWord [mkPLetter "F" []] @=? (parseUnsafe "  F  ")
      , testCase "Expressions"
          $ LWord [mkPLetter "F" [], mkPLetter "+" []] @=? (parseUnsafeWordExpr "F +")
      , testCase "Addition"
          $ LWord [mkPLetter "F" [ExprOp2 Sum (ExprVar "x") (ExprConst 1)]]
            @=? (parseUnsafeWordExpr "F(x+1)")
      , testCase "Subtraction"
          $ LWord [mkPLetter "F" [ExprOp2 Sum (ExprVar "x") (ExprOp2 Product (ExprConst 1) (ExprConst (-1)))]]
            @=? (parseUnsafeWordExpr "F(x-1)")
      , testCase "Multiplication"
          $ LWord [mkPLetter "F" [ExprOp2 Product (ExprVar "x") (ExprConst 1)]]
            @=? (parseUnsafeWordExpr "F(x*1)")
      , testCase "Division"
          $ LWord [mkPLetter "F" [ExprOp2 Fraction (ExprVar "x") (ExprConst 1)]]
            @=? (parseUnsafeWordExpr "F(x/1)")
      , testCase "Exponent"
          $ LWord [mkPLetter "F" [ExprOp2 Exponent (ExprVar "x") (ExprConst 1)]]
            @=? (parseUnsafeWordExpr "F(x^1)")
      , testCase "Parens"
          $ LWord [mkPLetter "F" [ExprOp2 Sum (ExprVar "x") (ExprVar "x")]]
            @=? (parseUnsafeWordExpr "F(((x+x)))")
      , testCase "Patterns"
          $ mkPLetter "F" ["x", "y"] @=? (parseUnsafePattern "F(x, y)")
      ]
    ]
  ]

-- ω : B(2)A(4, 4)
-- p1 : A(x, y) : y <= 3 → A(x ∗ 2, x + y)
-- p2 : A(x, y) : y > 3 → B(x)A(x/y, 0)
-- p3 : B(x) : x < 1 → C
-- p4 : B(x) : x >= 1 → B(x − 1)

mkPLetter s p = PLetter { letterSymbol = s, letterParams = p }

makePath :: S.AttributeValue
makePath = mkPath $ do
  m 0 0
  l 2 3
  l 4 5

makeTransform :: S.AttributeValue
makeTransform = rotate 50
