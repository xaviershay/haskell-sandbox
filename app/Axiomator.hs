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
  example = (Sum (Var "a") (Var "b"), Sum (Var "b") (Var "a")),
  implementation = f
}
  where
    f (Sum a b) = Right (Sum b a)
    f t = Left t

axiomAssociateSum = Axiom {
  description = "Associative law for addition",
  example = (
    Sum (Var "a") (Sum (Var "b") (Var "c")),
    Sum (Sum (Var "a") (Var "b")) (Var "c")
  ),
  implementation = f
}
  where
    f (Sum (Sum a b) c) = Right (Sum a (Sum b c))
    f (Sum a (Sum b c)) = Right (Sum (Sum a b) c)
    f t = Left t

axiomCommuteProduct = Axiom {
  description = "Commutative law for multiplication",
  example = (Product (Var "a") (Var "b"), Product (Var "b") (Var "a")),
  implementation = f
}
  where
    f (Product a b) = Right (Product b a)
    f t = Left t

axiomAssociateProduct = Axiom {
  description = "Associative law for multiplication",
  example = (
    Product (Var "a") (Product (Var "b") (Var "c")),
    Product (Product (Var "a") (Var "b")) (Var "c")
  ),
  implementation = f
}
  where
    f (Product (Product a b) c) = Right (Product a (Product b c))
    f (Product a (Product b c)) = Right (Product (Product a b) c)
    f t = Left t

axiomSumConst = Axiom {
  description = "Sum constants",
  example = (
    Sum (Const 1) (Const 2),
    (Const 3)
  ),
  implementation = f
}
  where
    f (Sum (Const a) (Const b)) = Right (Const $ a + b)
    f t = Left t

axiomMultiplyConst = Axiom {
  description = "Multiply constants",
  example = (
    Product (Const 2) (Const 3),
    (Const 6)
  ),
  implementation = f
}
  where
    f (Product (Const a) (Const b)) = Right (Const $ a * b)
    f (Exponent (Const a) (Const b)) = Right (Const $ a ^ b)
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
    (Sum (Sum (Const 0) (Var "a")) (Const 0)),
    (Var "a")
  ),
  implementation = f
}
  where
    f (Sum (Const 0) t) = Right t
    f (Sum t (Const 0)) = Right t
    f t = Left t

axiomIdentityProduct = Axiom {
  description = "Multiplicative identity",
  example = (
    (Product (Product (Const 1) (Var "a")) (Const 1)),
    (Var "a")
  ),
  implementation = f
}
  where
    f (Product (Const 1) t) = Right t
    f (Product t (Const 1)) = Right t
    f (Fraction t (Const 1)) = Right t
    f t = Left t

axiomNullExponent = Axiom {
  description = "Exponent of 0",
  example = (
    (Exponent (Var "a") (Const 0)),
    (Const 1)
  ),
  implementation = f
}
  where
    f (Exponent t (Const 0)) = Right (Const 1)
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
    (Series "k" (Var "a") (Var "k")),
    (Sum (Var "a") (Series "k" (Sum (Var "a") (Const 1)) (Var "k")))
  ),
  implementation = f
}
  where
    f (Series v i t) = Right $
      Sum
        (walk (instantiateVariable v i) t)
        (Series v (Sum i (Const 1)) t)
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
    f (Sum (Product p1l p1r) (Product p2l p2r))
      | p1l == p2l = Right $ Product p1l (Sum p1r p2r)
    f (Product pl (Sum sl sr)) = Right $ Sum (Product pl sl) (Product pl sr)
    f (Sum v@(Var{}) p@(Product{})) = f (Sum (Product v (Const 1)) p)
    f t = Left t

instantiateVariable :: String -> Term -> Term -> Term
instantiateVariable name value (Var vname) | name == vname = value
instantiateVariable _ _ t = t

allAxioms =
  [ axiomCommuteSum
  , axiomAssociateSum
  , axiomCommuteProduct
  , axiomAssociateProduct
  , axiomIdentitySum
  , axiomIdentityProduct
  , axiomNullExponent
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
    f (Sum (Const a) (Const b)) = Const $ a + b
    f (Product (Const a) (Const b)) = Const $ a * b
    f (Exponent a (Const 0)) = Const 1
    f (Exponent a (Const 1)) = a
    f (Exponent (Const a) (Const b)) = Const $ a ^ b
    f (Fraction a (Const 1)) = a
    f t@(Fraction (Const a) (Const b)) =
      case gcd a b of
        1 -> t
        n -> simplify $ Fraction (Const $ a `div` n) (Const $ b `div` n)
    f (Product (Const 1) a) = a
    f (Product a (Const 1)) = a
    f (Sum a (Const 0)) = a
    f (Sum (Const 0) a) = a
    f x = x

distribute t (Product a (Sum b c)) =
  let x = cancelTerm t $ Fraction a t in

  Product x $
    Sum
      (Product t b)
      (Product t c)

undistribute t (Sum a b) =
  Product t $
    Sum
      (cancelTerm t $ Fraction a t)
      (cancelTerm t $ Fraction b t)

cancelTerm :: Term -> Term -> Term
cancelTerm (Exponent x y) f@(Fraction (Exponent a b) (Exponent c d)) =
  case Fraction <$> numerator <*> denominator of
    Just x -> x
    Nothing -> f
  where
    numerator = if x == a then Just (Exponent a (Sum b (Product (Const (-1)) y))) else Nothing
    denominator = if x == c then Just (Exponent c (Sum d (Product (Const (-1)) y))) else Nothing

cancelTerm t f@(Fraction (Exponent{}) (Exponent{})) = cancelTerm (Exponent t (Const 1)) f
cancelTerm t (Fraction lhs@(Exponent{}) rhs) = cancelTerm t (Fraction lhs (Exponent rhs (Const 1)))
cancelTerm t (Fraction lhs rhs@(Exponent{})) = cancelTerm t (Fraction (Exponent lhs (Const 1)) rhs)
cancelTerm t f@(Fraction (Product a b) (Product c d)) =
    case Fraction <$> numerator <*> denominator of
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
cancelTerm t (Fraction l@(Product{}) r) = cancelTerm t (Fraction l (Product r (Const 1)))
cancelTerm t (Fraction l r@(Product{})) = cancelTerm t (Fraction l (Product (Const 1) r))
cancelTerm t (Fraction l r) = cancelTerm t (Fraction (Product (Const 1) l) (Product (Const 1) r))

data Crumb =
    LeftCrumb Term
  | RightCrumb Term
  deriving (Show)

type Crumbs = [Crumb]
type Zipper = (Term, Crumbs)

goLeft :: Zipper -> Zipper
goLeft (Sum l r, cs) = (l, LeftCrumb (Sum Hole r):cs)
goLeft (Product l r, cs) = (l, LeftCrumb (Product Hole r):cs)
goLeft (Fraction l r, cs) = (l, LeftCrumb (Fraction Hole r):cs)
goLeft (Exponent l r, cs) = (l, LeftCrumb (Exponent Hole r):cs)
goLeft (t, cs) = (Hole, cs)

goRight :: Zipper -> Zipper
goRight (Sum l r, cs) = (r, RightCrumb (Sum l Hole):cs)
goRight (Product l r, cs) = (r, RightCrumb (Product l Hole):cs)
goRight (Fraction l r, cs) = (r, RightCrumb (Fraction l Hole):cs)
goRight (Exponent l r, cs) = (r, RightCrumb (Exponent l Hole):cs)
goRight (Series v i t, cs) = (t, RightCrumb (Series v i Hole):cs)
goRight (Factorial t, cs) = (t, RightCrumb (Factorial Hole):cs)
goRight (t, cs) = (Hole, cs)

goUp :: Zipper -> Zipper
goUp (t, LeftCrumb (Sum _ r):cs) = (Sum t r, cs)
goUp (t, RightCrumb (Sum l _):cs) = (Sum l t, cs)
goUp (t, LeftCrumb (Product _ r):cs) = (Product t r, cs)
goUp (t, RightCrumb (Product l _):cs) = (Product l t, cs)

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

testF = head $ filterZip isConst (Sum (Const 3) (Sum (Const 1) (Const 4)), [])


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
termEqual (Sum a b) (Sum c d) = a `termEqual` c && b `termEqual` d
termEqual (Product a b) (Product c d) = a `termEqual` c && b `termEqual` d
termEqual (Fraction a b) (Fraction c d) = a `termEqual` c && b `termEqual` d
termEqual (Exponent a b) (Exponent c d) = a `termEqual` c && b `termEqual` d
termEqual (Series l1 a b) (Series l2 c d) = l1 == l2 && a `termEqual` c && b `termEqual` d
termEqual (Var a) (Var c) = a == c
termEqual (Const a) (Const c) = a == c
termEqual (Factorial a) (Factorial c) = a == c
termEqual _ _ = False

data Term =
  Hole |
  Const Integer |
  Sum Term Term |
  Product Term Term |
  Var String |
  Series String Term Term |
  Factorial Term |
  Fraction Term Term |
  Exponent Term Term
  deriving (Show, Eq)

instance IsString Term where
    fromString cs = parseUnsafe cs

walk :: (Term -> Term) -> Term -> Term
walk f (Sum a b) = f (Sum (walk f a) (walk f b))
walk f (Product a b) = f (Product (walk f a) (walk f b))
walk f (Series v i t) = f (Series v (walk f i) (walk f t))
walk f (Factorial t) = f (Factorial (walk f t))
walk f (Fraction a b) = f (Fraction (walk f a) (walk f b))
walk f (Exponent a b) = f (Exponent (walk f a) (walk f b))
walk f t@(Const{}) = f t
walk f t@(Var{}) = f t

data Matcher =
  RootMatcher
  | LeftMatcher Term
  | AllMatcher
  | SeriesMatcher String

walkMatched :: Matcher -> (Term -> Either Term Term) -> Term -> Either Term Term
walkMatched m f t = Right $ walk f' t
  where
    f' :: Term -> Term
    f' t = if matcherApplies m t then
             case f t of
               Right t' -> t'
               Left t' -> t'
           else
             t

matcherApplies :: Matcher -> Term -> Bool
matcherApplies (LeftMatcher x) (Sum a _) = x == a
matcherApplies (LeftMatcher x) (Product a _) = x == a
matcherApplies (SeriesMatcher v) (Series v' _ _) = v == v'
matcherApplies AllMatcher _ = True
matcherApplies _ _ = False

precedence (Factorial{}) = 40
precedence (Exponent{}) = 30
precedence (Product{}) = 20
precedence (Fraction{}) = 20
precedence (Sum{}) = 10
precedence _ = 99

maybeBrackets parent child = let inner = toUnicode child in
  if precedence child < precedence parent then
    "(" <> inner <> ")"
  else
    inner

toUnicode Hole             = "_"
toUnicode (Const a)        = show a
toUnicode (Var a)          = a
toUnicode t@(Sum a b)      = maybeBrackets t a <> " + " <> maybeBrackets t b
toUnicode t@(Product a b)  =
  let operator = case (a, b) of
                   (_, Const{}) -> "⋅"
                   _            -> ""
  in maybeBrackets t a <> operator  <> maybeBrackets t b

toUnicode t@(Factorial a)  = maybeBrackets t a <> "!"
toUnicode t@(Fraction a b) = maybeBrackets t a <> "/" <> maybeBrackets t b
toUnicode t@(Exponent a b) = maybeBrackets t a <> "^" <> maybeBrackets t b
toUnicode (Series v i t)   =
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

applyOld :: AppEff effs => Matcher -> Axiom -> Eff effs ()
applyOld m axiom = do
  (Env t) <- get

  -- TODO: Handle error
  case walkMatched m (implementation axiom) t of
    Right t' -> do
      let t'' =  t'
    --  walk (
    --              ignoreError . implementation axiomSumConst .
    --              ignoreError . implementation axiomMultiplyConst .
    --              ignoreError . implementation axiomNullExponent
    --            ) t'
      tell [(t'', axiom)]

      put (Env t'')
    Left t' -> do
      error $ "couldn't apply " <> description axiom <> " to " <> toAscii t' <> " (full term is " <> toAscii t <> ")"

--body = runProcess (Sum (Var "x") (Sum (Const 2) (Const 3))) $ do
--  apply RootMatcher axiomCommuteSum
--  apply RootMatcher axiomAssociateSum
--  apply RootMatcher axiomAssociateSum
--  apply (LeftMatcher (Const 2)) axiomSumConst

--body = runProcess (Series "k" (Const 0) (Sum (Var "x") (Var "k"))) $ do
--  apply (SeriesMatcher "k") axiomStepSeries
--  apply (SeriesMatcher "k") axiomStepSeries

--body = runProcess (Product (Const 2) (Product (Const 3) (Const 4))) $ do
--  apply RootMatcher axiomCommuteProduct
--  apply (LeftMatcher (Const 3)) axiomMultiplyConst

--body = runProcess (Sum (Var "x") (Product (Var "x") (Var "x"))) $ do
--  apply RootMatcher axiomDistribute
--  apply RootMatcher axiomDistribute
  --apply RootMatcher axiomDistribute

matchSeries (Series{}) = True
matchSeries _ = False


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

table = [ [postfix "!" Factorial, series "S" ]
        , [binary "^" Exponent AssocLeft ]
        , [binary "*" Product AssocLeft, binary "/" Fraction AssocLeft, binary "" Product AssocLeft]
        , [binary "+" Sum AssocLeft ]
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

    return $ Series v i

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
e_to t = (Series "k" (Const 0) (Fraction (Exponent t (Var "k")) (Factorial (Var "k"))))
--cos_x = (Series "m" (Const 0) (Product (Exponent (Const (-1)) (Var "m")) (Fraction (Exponent (Var "x") (Product (Const 2) (Var "m"))) (Factorial (Product (Const 2) (Var "m"))))))

cos_x = parseUnsafe "S[m=0]((-1)^m*(x^(2*m))/(2*m)!)"
--body = runProcess (e_to cos_x) $ do
--  apply (SeriesMatcher "m") axiomStepSeries
--  --apply (SeriesMatcher "k") axiomStepSeries
--  --apply AllMatcher axiomFactorialConst
--  --apply AllMatcher axiomIdentityProduct
--  --apply (SeriesMatcher "m") axiomStepSeries
--  --apply (SeriesMatcher "k") axiomStepSeries
--  --apply AllMatcher axiomFactorialConst
--  --apply AllMatcher axiomIdentityProduct
--
--  highlightTerms matchSeries
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
--main = defaultMain tests
main = runSolution solution
--main = putStrLn $ show testF

solution = do
  initial "x(y+z)"
  focus "y+_" $ apply axiomCommuteSum
--solution = do
--  initial "(sin(x+h)-sin(x))/h"
--
--  focus "sin(x+h)" $ apply (axiomIdentity "sin(a+b)" "sin(a)cos(b) + sin(b)cos(a)")
--  focus "_-sin(x)" $ apply axiomCommuteSum
--  focus "sin(x)*_-sin(x)" $ apply axiomDistributeSum
--  focus "_/h" $ apply axiomDistributeProduct
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
