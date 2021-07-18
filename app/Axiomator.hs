{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad (mfilter)
import Data.String (IsString(..))
import Debug.Trace (trace, traceM)
import Data.List
import Data.Hashable
import GHC.Generics hiding (Infix, Prefix)
import qualified Data.Tuple
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Monoid ((<>))
import Control.Monad (msum, forM_)

import Text.Parsec hiding (State(..))
import Text.Parsec.Expr
import qualified Text.Parsec.Token    as Tok
import qualified Text.Parsec.Language as Tok

import Control.Monad.Freer (Eff, Members, Member, run, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)
import Control.Monad.Freer.State (State(..), get, gets, put, runState)
import Control.Monad.Freer.Writer (Writer(..), tell, runWriter)
import Test.Tasty
import Test.Tasty.HUnit
--data Axiom =
--  CommuteSum |
--  AssociateSumLR |
--  AssociateSumRL |
--  CombineConst

-- TODO: replace with Text package
replace a b = map $ maybe b id . mfilter (/= a) . Just

data Axiom = Axiom {
  description :: String,
  example :: (Term, Term),
  implementation :: Term -> Either Term Term
}

instance Eq Axiom where
  a == b = description a == description b

instance Show Axiom where
  show (Axiom { description = d }) = d

axiomCommuteSum = Axiom {
  description = "Commutative law for addition",
  example = ("a+b", "b+a"),
  implementation = f
}
  where
    f (BinaryOp Sum a b) = Right (BinaryOp Sum b a)
    f t = Left t

axiomAssociateSum = Axiom {
  description = "Associative law for addition",
  example = ("a+(b+c)", "(a+b)+c"),
  implementation = f
}
  where
    f (BinaryOp Sum (BinaryOp Sum a b) c) = Right (BinaryOp Sum a (BinaryOp Sum b c))
    f (BinaryOp Sum a (BinaryOp Sum b c)) = Right (BinaryOp Sum (BinaryOp Sum a b) c)
    f t = Left t

axiomCommuteBinaryOp Product = Axiom {
  description = "Commutative law for multiplication",
  example = (BinaryOp Product (Var "a") (Var "b"), BinaryOp Product (Var "b") (Var "a")),
  implementation = f
}
  where
    f (BinaryOp Product a b) = Right (BinaryOp Product b a)
    f t = Left t

axiomAssociateBinaryOp Product = Axiom {
  description = "Associative law for multiplication",
  example = (
    BinaryOp Product (Var "a") (BinaryOp Product (Var "b") (Var "c")),
    BinaryOp Product (BinaryOp Product (Var "a") (Var "b")) (Var "c")
  ),
  implementation = f
}
  where
    f (BinaryOp Product (BinaryOp Product a b) c) = Right (BinaryOp Product a (BinaryOp Product b c))
    f (BinaryOp Product a (BinaryOp Product b c)) = Right (BinaryOp Product (BinaryOp Product a b) c)
    f t = Left t

axiomSumConst = Axiom {
  description = "Sum constants",
  example = ("1+2", "3"),
  implementation = f
}
  where
    f (BinaryOp Sum (Const a) (Const b)) = Right (Const $ a + b)
    f t = Left t

axiomMultiplyConst = Axiom {
  description = "Multiply constants",
  example = (
    BinaryOp Product (Const 2) (Const 3),
    (Const 6)
  ),
  implementation = f
}
  where
    f (BinaryOp Product (Const a) (Const b)) = Right (Const $ a * b)
    f (BinaryOp Exponent (Const a) (Const b)) = Right (Const $ a ^ b)
    f t = Left t

axiomFactorialConst = Axiom {
  description = "Factorial constants",
  example = (
    Factorial (Const 3),
    (Const 6)
  ),
  implementation = f
}
  where
    f (Factorial (Const x)) = Right . Const $ factorial x
    f t = Left t

factorial 0 = 1
factorial x = x * factorial (x-1)

axiomIdentitySum = Axiom {
  description = "Additive identity",
  example = (
    "0+a+0",
    "a"
  ),
  implementation = f
}
  where
    f (BinaryOp Sum (Const 0) t) = Right t
    f (BinaryOp Sum t (Const 0)) = Right t
    f t = Left t

axiomIdentityBinaryOp Product = Axiom {
  description = "Multiplicative identity",
  example = (
    (BinaryOp Product (BinaryOp Product (Const 1) (Var "a")) (Const 1)),
    (Var "a")
  ),
  implementation = f
}
  where
    f (BinaryOp Product (Const 1) t) = Right t
    f (BinaryOp Product t (Const 1)) = Right t
    f (BinaryOp Fraction t (Const 1)) = Right t
    f t = Left t

axiomNullBinaryOp Exponent = Axiom {
  description = "BinaryOp Exponent of 0",
  example = (
    (BinaryOp Exponent (Var "a") (Const 0)),
    (Const 1)
  ),
  implementation = f
}
  where
    f (BinaryOp Exponent t (Const 0)) = Right (Const 1)
    f t = Left t

axiomIdentity = Axiom {
  description = "Identity",
  example = (
    (Var "a"),
    (Var "a")
  ),
  implementation = f
}
  where
    f t = Right t

axiomStepSeries = Axiom {
  description = "Step series",
  example = (
    (BinaryOp (Series "k") (Var "a") (Var "k")),
    (BinaryOp Sum (Var "a") (BinaryOp (Series "k") (BinaryOp Sum (Var "a") (Const 1)) (Var "k")))
  ),
  implementation = f
}
  where
    f (BinaryOp (Series v) i t) = Right $
      BinaryOp Sum
        (walk (instantiateVariable v i) t)
        (BinaryOp (Series v) (BinaryOp Sum i (Const 1)) t)
    f t = Left t

axiomDistribute = Axiom {
  description = "Distributive law",
  example = (
    parseUnsafe "a*(b+c)",
    parseUnsafe "(a*b)+(a*c)"
  ),
  implementation = f
}
  where
    f (BinaryOp Sum (BinaryOp Product p1l p1r) (BinaryOp Product p2l p2r))
      | p1l == p2l = Right $ BinaryOp Product p1l (BinaryOp Sum p1r p2r)
    f (BinaryOp Product pl (BinaryOp Sum sl sr)) = Right $ BinaryOp Sum (BinaryOp Product pl sl) (BinaryOp Product pl sr)
    f (BinaryOp Sum v@(Var{}) p@(BinaryOp Product _ _)) = f (BinaryOp Sum (BinaryOp Product v (Const 1)) p)
    f t = Left t

instantiateVariable :: String -> Term -> Term -> Term
instantiateVariable name value (Var vname) | name == vname = value
instantiateVariable _ _ t = t

allAxioms =
  [ axiomCommuteSum
  , axiomAssociateSum
  , axiomIdentitySum
  , axiomDistribute
  , axiomStepSeries
  , axiomSumConst
  , axiomMultiplyConst
  , axiomFactorialConst
  ]

p = parseUnsafe
ps = putStrLn . toAscii

simplify t = walk f t
  where
    f (Negate (Const a)) = Const (-a)
    f (Negate (Negate a)) = a
    f (BinaryOp Sum (Const a) (Const b)) = Const $ a + b
    f (BinaryOp Product (Const a) (Const b)) = Const $ a * b
    f (BinaryOp Exponent a (Const 0)) = Const 1
    f (BinaryOp Exponent a (Const 1)) = a
    f (BinaryOp Exponent (Const a) (Const b)) = Const $ a ^ b
    f (BinaryOp Fraction a (Const 1)) = a
    f (BinaryOp Fraction a (Const (-1))) = f $ Negate a
    f t@(BinaryOp Fraction (Const a) (Const b)) =
      case gcd a b of
        1 -> t
        n -> simplify $ BinaryOp Fraction (Const $ a `div` n) (Const $ b `div` n)
    f (BinaryOp Product (Const 1) a) = a
    f (BinaryOp Product a (Const 1)) = a
    f (BinaryOp Product (Const (-1)) a) = f $ Negate a
    f (BinaryOp Product a (Const (-1))) = f $ Negate a
    f (BinaryOp Sum a (Const 0)) = a
    f (BinaryOp Sum (Const 0) a) = a
    f x = x

distribute t (BinaryOp Product a (BinaryOp Sum b c)) =
  let x = cancelTerm t $ BinaryOp Fraction a t in

  BinaryOp Product x $
    BinaryOp Sum
      (BinaryOp Product t b)
      (BinaryOp Product t c)

undistribute t (BinaryOp Sum a b) =
  BinaryOp Product t $
    BinaryOp Sum
      (cancelTerm t $ BinaryOp Fraction a t)
      (cancelTerm t $ BinaryOp Fraction b t)

cancelTerm :: Term -> Term -> Term
cancelTerm (BinaryOp Exponent x y) f@(BinaryOp Fraction (BinaryOp Exponent a b) (BinaryOp Exponent c d)) =
  case BinaryOp Fraction <$> numerator <*> denominator of
    Just x -> x
    Nothing -> f
  where
    numerator = if x == a then Just (BinaryOp Exponent a (BinaryOp Sum b (BinaryOp Product (Const (-1)) y))) else Nothing
    denominator = if x == c then Just (BinaryOp Exponent c (BinaryOp Sum d (BinaryOp Product (Const (-1)) y))) else Nothing

cancelTerm t f@(BinaryOp Fraction (BinaryOp Exponent _ _) (BinaryOp Exponent _ _)) = cancelTerm (BinaryOp Exponent t (Const 1)) f
cancelTerm t (BinaryOp Fraction lhs@(BinaryOp Exponent _ _) rhs) = cancelTerm t (BinaryOp Fraction lhs (BinaryOp Exponent rhs (Const 1)))
cancelTerm t (BinaryOp Fraction lhs rhs@(BinaryOp Exponent _ _)) = cancelTerm t (BinaryOp Fraction (BinaryOp Exponent lhs (Const 1)) rhs)
cancelTerm t f@(BinaryOp Fraction (BinaryOp Product a b) (BinaryOp Product c d)) =
    case BinaryOp Fraction <$> numerator <*> denominator of
      Just x -> x
      Nothing -> f
  where
    numerator =
      case (a, b) of
        (a, b) | a == t -> Just b
        (a, b) | b == t -> Just a
        _               -> Nothing
    denominator =
      case (c, d) of
        (c, d) | c == t -> Just d
        (c, d) | d == t -> Just c
        _               -> Nothing
cancelTerm t (BinaryOp Fraction l@(BinaryOp Product _ _) r) = cancelTerm t (BinaryOp Fraction l (BinaryOp Product r (Const 1)))
cancelTerm t (BinaryOp Fraction l r@(BinaryOp Product _ _)) = cancelTerm t (BinaryOp Fraction l (BinaryOp Product (Const 1) r))
cancelTerm t (BinaryOp Fraction l r) = cancelTerm t (BinaryOp Fraction (BinaryOp Product (Const 1) l) (BinaryOp Product (Const 1) r))

data Crumb =
    LeftCrumb Term
  | RightCrumb Term
  deriving (Show)

type Crumbs = [Crumb]
type Zipper = (Term, Crumbs)

goLeft :: Zipper -> Zipper
goLeft (BinaryOp op l r, cs) = (l, LeftCrumb (BinaryOp op Hole r):cs)
goLeft (t, cs) = (Hole, cs)

goRight :: Zipper -> Zipper
goRight (BinaryOp op l r, cs) = (r, RightCrumb (BinaryOp op l Hole):cs)
goRight (Factorial t, cs) = (t, RightCrumb (Factorial Hole):cs)
goRight (t, cs) = (Hole, cs)

goUp :: Zipper -> Zipper
goUp (t, LeftCrumb (BinaryOp op _ r):cs) = (BinaryOp op t r, cs)
goUp (t, RightCrumb (BinaryOp op l _):cs) = (BinaryOp op l t, cs)

goRoot :: Zipper -> Term
goRoot (t, []) = t
goRoot z = goRoot . goUp $ z

filterZip :: (Term -> Bool) -> Zipper -> [Zipper]
filterZip f (Hole, _) = []
filterZip f z@(t, cs) = do
  let currentNode = if f t then [(t, cs)] else []
      lhs = filterZip f (goLeft z)
      rhs = filterZip f (goRight z)
    in currentNode ++ lhs ++ rhs

isConst (Const t) = True
isConst _ = False

locate :: Term -> Term -> Maybe Zipper
locate needle haystack = locate' needle (haystack, [])

locate' :: Term -> Zipper -> Maybe Zipper
locate' Hole z = Just z
locate' _ (Hole, _) = Nothing
locate' a z@(b, _) | a `termEqual` b = Just z
locate' a z = msum [locate' a (goLeft z), locate' a (goRight z)]

termEqual :: Term -> Term -> Bool
termEqual Hole _ = True
termEqual _ Hole = True
termEqual (BinaryOp op1 a b) (BinaryOp op2 c d) = op1 == op2 && a `termEqual` c && b `termEqual` d
termEqual (Var a) (Var c) = a == c
termEqual (Const a) (Const c) = a == c
termEqual (Factorial a) (Factorial c) = a == c
termEqual _ _ = False

data BinaryTerm = Sum | Product | Fraction | Exponent | Series String
  deriving (Show, Eq)

data Term =
  Hole |
  Const Integer |
  BinaryOp BinaryTerm Term Term |
  Var String |
  Factorial Term |
  Negate Term
  deriving (Show, Eq)

instance IsString Term where
    fromString cs = parseUnsafe cs

walk :: (Term -> Term) -> Term -> Term
walk f (BinaryOp op a b) = f (BinaryOp op (walk f a) (walk f b))
walk f (Factorial t) = f (Factorial (walk f t))
walk f t@(Const{}) = f t
walk f t@(Var{}) = f t
walk f (Negate t) = f (Negate $ walk f t)

--data Matcher =
--  RootMatcher
--  | LeftMatcher Term
--  | AllMatcher
--  | BinaryOp SeriesMatcher String
--
--walkMatched :: Matcher -> (Term -> Either Term Term) -> Term -> Either Term Term
--walkMatched m f t = Right $ walk f' t
--  where
--    f' :: Term -> Term
--    f' t = if matcherApplies m t then
--             case f t of
--               Right t' -> t'
--               Left t' -> t'
--           else
--             t

--matcherApplies :: Matcher -> Term -> Bool
--matcherApplies (LeftMatcher x) (Sum a _) = x == a
--matcherApplies (LeftMatcher x) (BinaryOp Product a _) = x == a
--matcherApplies (BinaryOp SeriesMatcher v) (BinaryOp Series v' _ _) = v == v'
--matcherApplies AllMatcher _ = True
--matcherApplies _ _ = False

precedence (Factorial{}) = 40
precedence (BinaryOp Exponent _ _) = 30
precedence (BinaryOp Product _ _) = 20
precedence (BinaryOp Fraction _ _) = 20
precedence (BinaryOp Sum _ _) = 10
precedence _ = 99

maybeBrackets parent child = let inner = toUnicode child in
  if precedence child < precedence parent then
    "(" <> inner <> ")"
  else
    inner

toUnicode Hole             = "_"
toUnicode (Const a)        = show a
toUnicode (Var a)          = a
toUnicode t@(BinaryOp Sum a (Negate b))      = maybeBrackets t a <> " - " <> maybeBrackets t b
toUnicode t@(BinaryOp Sum a b) = maybeBrackets t a <> " + " <> maybeBrackets t b
toUnicode t@(BinaryOp Product a b)  =
  let operator = case (a, b) of
                   (_, Const{}) -> "⋅"
                   (_, Negate{}) -> "⋅"
                   _            -> ""
  in maybeBrackets t a <> operator  <> maybeBrackets t b

toUnicode t@(Factorial a)  = maybeBrackets t a <> "!"
toUnicode t@(BinaryOp Fraction a b) = maybeBrackets t a <> "/" <> maybeBrackets t b
toUnicode t@(BinaryOp Exponent a b) = maybeBrackets t a <> "^" <> maybeBrackets t b
toUnicode t@(Negate a) = "-" <> maybeBrackets t a
toUnicode (BinaryOp (Series v) i t)   =
  "Σ[" <> v <> " = " <> toUnicode i <> "](" <> toUnicode t <> ")"

toAscii :: Term -> String
toAscii = replace 'Σ' 'S' . replace '⋅' '*' . toUnicode

data Env = Env Term deriving (Show)

type Log = [(Term, Axiom)]
type AppEff effs = Members '[ Writer Log, State Env ] effs

ignoreError :: Either a a -> a
ignoreError (Left x) = x
ignoreError (Right x) = x

apply :: AppEff effs => Axiom -> Eff effs ()
apply axiom = do
  Env t <- get

  case (implementation axiom) t of
    Right t' -> do
      tell [(t', axiom)]
      put (Env t')
    Left t' -> do
      error $ "couldn't apply " <> description axiom <> " to " <> toUnicode t' <> " (full term is " <> toUnicode t <> ")"

--applyOld :: AppEff effs => Matcher -> Axiom -> Eff effs ()
--applyOld m axiom = do
--  (Env t) <- get
--
--  -- TODO: Handle error
--  case walkMatched m (implementation axiom) t of
--    Right t' -> do
--      let t'' =  t'
--    --  walk (
--    --              ignoreError . implementation axiomSumConst .
--    --              ignoreError . implementation axiomMultiplyConst .
--    --              ignoreError . implementation axiomNullBinaryOp Exponent
--    --            ) t'
--      tell [(t'', axiom)]
--
--      put (Env t'')
--    Left t' -> do
--      error $ "couldn't apply " <> description axiom <> " to " <> toAscii t' <> " (full term is " <> toAscii t <> ")"

--body = runProcess (Sum (Var "x") (Sum (Const 2) (Const 3))) $ do
--  apply RootMatcher axiomCommuteSum
--  apply RootMatcher axiomAssociateSum
--  apply RootMatcher axiomAssociateSum
--  apply (LeftMatcher (Const 2)) axiomSumConst

--body = runProcess (BinaryOp Series "k" (Const 0) (Sum (Var "x") (Var "k"))) $ do
--  apply (BinaryOp SeriesMatcher "k") axiomStepBinaryOp Series
--  apply (BinaryOp SeriesMatcher "k") axiomStepBinaryOp Series

--body = runProcess (BinaryOp Product (Const 2) (BinaryOp Product (Const 3) (Const 4))) $ do
--  apply RootMatcher axiomCommuteBinaryOp Product
--  apply (LeftMatcher (Const 3)) axiomMultiplyConst

--body = runProcess (Sum (Var "x") (BinaryOp Product (Var "x") (Var "x"))) $ do
--  apply RootMatcher axiomDistribute
--  apply RootMatcher axiomDistribute
  --apply RootMatcher axiomDistribute

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = Tok.emptyDef
      { Tok.reservedOpNames = ["+", "!", "^", "/", "*"]
      , Tok.reservedNames   = []
      , Tok.identStart      = letter
      }

reservedOp = Tok.reservedOp lexer
whiteSpace = Tok.whiteSpace lexer

parens = between (char '(' <* whiteSpace) (char ')')

termExpr = (parens expr
             <|> Const . read <$> many1 (oneOf ['0'..'9'])
             <|> Var . replicate 1 <$> oneOf ['a'..'z']
             <|> (char '_' >> return Hole)
           ) <* whiteSpace

table = [ [postfix "!" Factorial, series "S", prefix "-" Negate ]
        , [binary "^" (BinaryOp Exponent) AssocLeft ]
        , [binary "*" (BinaryOp Product) AssocLeft, binary "/" (BinaryOp Fraction) AssocLeft, binary "" (BinaryOp Product) AssocLeft]
        , [binary "+" (BinaryOp Sum) AssocLeft, binary "-" (\a b -> BinaryOp Sum a (Negate b)) AssocLeft ]
        ]

series op = Prefix $
  do
    string op
    char '['
    v <- replicate 1 <$> oneOf ['a'..'z']
    whiteSpace
    char '='
    whiteSpace
    i <- expr
    char ']'

    return $ BinaryOp (Series v) i

prefix name fun = Prefix (do { reservedOp name; return fun })
postfix name fun = Postfix (do { reservedOp name; return fun })
binary name fun assoc = Infix (do { reservedOp name; return fun}) assoc

expr = buildExpressionParser table (whiteSpace *> termExpr)

parseUnsafe input =
  case parse expr input input of
    Right x -> x
    Left x -> error $ "error parsing: " <> input

highlightTerms m = do
  Env t <- get

  let ms = filterZip m (t, [])

  traceM "====="
  forM_ ms $ \(t',_) -> do
    traceM . toAscii $ t'
    traceM "---"

-- TODO: Handle variable aliasing properly for nested series
e_to t = (BinaryOp (Series "k") (Const 0) (BinaryOp Fraction (BinaryOp Exponent t (Var "k")) (Factorial (Var "k"))))
--cos_x = (BinaryOp Series "m" (Const 0) (BinaryOp Product (BinaryOp Exponent (Const (-1)) (Var "m")) (BinaryOp Fraction (BinaryOp Exponent (Var "x") (BinaryOp Product (Const 2) (Var "m"))) (Factorial (BinaryOp Product (Const 2) (Var "m"))))))

cos_x = parseUnsafe "S[m=0]((-1)^m*(x^(2*m))/(2*m)!)"
--body = runProcess (e_to cos_x) $ do
--  apply (BinaryOp SeriesMatcher "m") axiomStepBinaryOp Series
--  --apply (BinaryOp SeriesMatcher "k") axiomStepBinaryOp Series
--  --apply AllMatcher axiomFactorialConst
--  --apply AllMatcher axiomIdentityBinaryOp Product
--  --apply (BinaryOp SeriesMatcher "m") axiomStepBinaryOp Series
--  --apply (BinaryOp SeriesMatcher "k") axiomStepBinaryOp Series
--  --apply AllMatcher axiomFactorialConst
--  --apply AllMatcher axiomIdentityBinaryOp Product
--
--  highlightTerms matchBinaryOp Series
--  --apply RootMatcher axiomAssociateSum

printAxioms axioms = do
  let paddingIndex = length (show $ length axioms)
  let paddingDesc = maximum . map (length . description) $ axioms
  forM_ (zip axioms [1..]) $ \(axiom, i) -> do
    putStr (show i)
    putStr ". "
    putStr $ replicate (paddingIndex - length (show i)) ' '
    putStr (description axiom)
    putStr ": "
    putStr $ replicate (paddingDesc - length (description axiom)) ' '
    let (lhs, rhs) = example axiom
    putStr $ toAscii lhs
    putStr " = "
    putStrLn $ toAscii rhs

initial :: AppEff effs => Term -> Eff effs ()
initial t = put (Env t)

focus :: AppEff effs => Term -> Eff effs () -> Eff effs ()
focus t m = do
  Env oldT <- get

  case locate t oldT of
    Just (t', cs) -> do
      put . Env $ t'
      m
      Env newT <- get

      put . Env $ goRoot (newT, cs)
    Nothing -> error $ "Could not focus: " <> show t

runSolution :: Eff '[ Writer Log, State Env] a -> IO ()
runSolution m = do
  let (_, log) = runApp (Env "_") m
  let usedAxioms = nub (map snd log)
  printAxioms usedAxioms
  putStrLn ""

  let paddingT = maximum $ map (length . toAscii . fst) log
  forM_ log $ \(t, axiom) -> do
    putStr (toAscii t)
    putStr $ replicate (paddingT - length (toAscii t)) ' '
    putStrLn $ " ; " <> description axiom

runProcess t m = do
  let (_, log) = runApp (Env t) m
  let usedAxioms = nub (map snd log)
  printAxioms usedAxioms
  putStrLn ""

  let paddingT = maximum $ map (length . toAscii . fst) log
  putStrLn . toAscii $ t
  forM_ log $ \(t, axiom) -> do
    putStr (toAscii t)
    putStr $ replicate (paddingT - length (toAscii t)) ' '
    putStrLn $ " ; " <> description axiom

--main = body
main = defaultMain tests
--main = runSolution solution
--main = putStrLn $ show testF

--solution = do
--  initial "x(y+z)"
--  focus "y+_" $ apply axiomCommuteSum
--solution = do
--  initial "lim[h->0]((sin(x+h)-sin(x))/h)"
--
--  focus "sin(x+h)" $ apply (axiomIdentity "sin(a+b)" "sin(a)cos(b) + sin(b)cos(a)")
--  focus "_-sin(x)" $ apply axiomCommuteSum
--  focus "sin(x)*_-sin(x)" $ apply axiomDistributeSum
--  focus "_/h" $ apply axiomDistributeBinaryOp Product
--  focus "lim[h->_](_+_)" $ apply axiomDistributeLimit
--  focus "lim[h->_](sin(x)_)" apply (axiomFactor "sin(x)")
--  focus "lim[h->_](cos(x)_)" apply (axiomFactor "cos(x)")
--  focus "lim[h->_](sin(h)_)" apply (axiomIdentity "lim[a->0](sin(a)/a)" "1")
--  focus "lim[h->_](cos(h)_)" apply (axiomIdentity "lim[a->0]((cos(a)-1)/a)" "0")
--  apply axiomZero
--  apply axiomIdentity

validate :: (Term -> Term) -> Term -> Term -> TestTree
validate f input expected =
  testCase (toUnicode input <> " = " <> toUnicode expected) $ (toAscii $ f input) @?=(toAscii $ expected)

validateAll :: TestName -> (Term -> Term) -> [(Term, Term)] -> TestTree
validateAll name f = testGroup name . map (uncurry $ validate f)

toAsciiTests =
  testGroup "toAscii (bracket reduction)" . map f $
    [ ("a+b", "a + b")
    , ("a+b+c", "a + b + c")
    , ("a+(bc)", "a + bc")
    , ("a*b+c)", "ab + c")
    , ("(a+b)*c", "(a + b)c")
    , ("abc", "abc")
    , ("a+b/c", "a + b/c")
    , ("(a+b)/c", "(a + b)/c")
    , ("a^2", "a^2")
    , ("(a+b)^(cd)", "(a + b)^(cd)")
    , ("(a+b)^c*d", "(a + b)^cd")
    , ("2a!", "2a!")
    , ("(2a)!", "(2a)!")
    ]

  where
    f :: (String, String) -> TestTree
    f (input, expected) = testCase (input <> " = " <> expected) $ expected @=? toAscii (parseUnsafe input)

tests = testGroup "Axioms"
  [ toAsciiTests
  , validateAll "simplify" simplify
    [ ("a + 0", "a")
    , ("0 + a", "a")
    , ("a*1", "a")
    , ("1*a", "a")
    , ("a/1", "a")
    , ("a^1", "a")
    , ("1+2", "3")
    , ("1+2+3", "6")
    , ("2*3", "6")
    , ("2*3*4", "24")
    , ("2*3+4", "10")
    , ("a^0", "1")
    , ("2^2", "4")
    , ("4/2", "2")
    , ("14/8", "7/4")
    , ("3-2", "1")
    , ("-1+2", "1")
    , ("-1-2", "-3")
    , ("3*(-2)", "-6")
    , ("-3*2", "-6")
    , ("-3*(-2)", "6")
    , ("-(-1)", "1")
    , ("-1*(-x)", "x")
    , ("-x*(-1)", "x")
    , ("-x/(-1)", "x")
    ]
  , validateAll "distribute \"a\"" (simplify . distribute "a") $
      [ ("a(b+c)", "ab+ac")
      , ("2a(b+c)", "2(ab+ac)")
      ]
  , validateAll "undistribute \"a\"" (simplify . undistribute "a")
      [ ("ab+ac", "a(b+c)")
      , ("ba+ac", "a(b+c)")
      , ("ab+ca", "a(b+c)")
      , ("ba+ca", "a(b+c)")
      , ("ab+a", "a(b+1)")
      , ("ba+a", "a(b+1)")
      , ("a+ab", "a(1+b)")
      , ("a+ba", "a(1+b)")
      , ("b+c", "a*(b/a+c/a)")
      ]
    , testGroup "cancelTerm (exponents)"
      [ validate (simplify . cancelTerm "x^1") "x^2/x^1" "x"
      , validate (simplify . cancelTerm "x") "x^2/x^1" "x"
      , validate (simplify . cancelTerm "x") "x^2/x" "x"
      , validate (simplify . cancelTerm "x") "x/x^1" "1"
      , validate (simplify . cancelTerm "x") "2x/x" "2"
      ]
  ]

runApp :: Env -> Eff '[ Writer Log, State Env] a -> (Term, Log)
runApp env m = do
  let ((_, log), (Env t)) = run . runState env . runWriter $ m

  (t, log)
