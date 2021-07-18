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

data Term1 = Factorial | Negate | Function String
  deriving (Show, Eq)
data Term2 = Sum | Product | Fraction | Exponent | Series String | Limit String
  deriving (Show, Eq)

data Term =
  Hole |
  Const Integer |
  Var String |
  Op1 Term1 Term |
  Op2 Term2 Term Term
  deriving (Show, Eq)

data Crumb =
    LeftCrumb Term
  | RightCrumb Term
  deriving (Show)

type Crumbs = [Crumb]
type Zipper = (Term, Crumbs)

allowedFunctionNames =
  [ "sin"
  , "cos"
  , "tan"
  , "f"
  , "g"
  ]

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
    f (Op2 Sum a b) = Right (Op2 Sum b a)
    f t = Left t

axiomAssociateSum = Axiom {
  description = "Associative law for addition",
  example = ("a+(b+c)", "(a+b)+c"),
  implementation = f
}
  where
    f (Op2 Sum (Op2 Sum a b) c) = Right (Op2 Sum a (Op2 Sum b c))
    f (Op2 Sum a (Op2 Sum b c)) = Right (Op2 Sum (Op2 Sum a b) c)
    f t = Left t

axiomCommuteProduct = Axiom {
  description = "Commutative law for multiplication",
  example = (Op2 Product (Var "a") (Var "b"), Op2 Product (Var "b") (Var "a")),
  implementation = f
}
  where
    f (Op2 Product a b) = Right (Op2 Product b a)
    f t = Left t

axiomAssociateProduct = Axiom {
  description = "Associative law for multiplication",
  example = (
    Op2 Product (Var "a") (Op2 Product (Var "b") (Var "c")),
    Op2 Product (Op2 Product (Var "a") (Var "b")) (Var "c")
  ),
  implementation = f
}
  where
    f (Op2 Product (Op2 Product a b) c) = Right (Op2 Product a (Op2 Product b c))
    f (Op2 Product a (Op2 Product b c)) = Right (Op2 Product (Op2 Product a b) c)
    f t = Left t

axiomSumConst = Axiom {
  description = "Sum constants",
  example = ("1+2", "3"),
  implementation = f
}
  where
    f (Op2 Sum (Const a) (Const b)) = Right (Const $ a + b)
    f t = Left t

axiomMultiplyConst = Axiom {
  description = "Multiply constants",
  example = (
    Op2 Product (Const 2) (Const 3),
    (Const 6)
  ),
  implementation = f
}
  where
    f (Op2 Product (Const a) (Const b)) = Right (Const $ a * b)
    f (Op2 Exponent (Const a) (Const b)) = Right (Const $ a ^ b)
    f t = Left t

axiomFactorialConst = Axiom {
  description = "Factorial constants",
  example = (
    Op1 Factorial (Const 3),
    (Const 6)
  ),
  implementation = f
}
  where
    f (Op1 Factorial (Const x)) = Right . Const $ factorial x
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
    f (Op2 Sum (Const 0) t) = Right t
    f (Op2 Sum t (Const 0)) = Right t
    f t = Left t

axiomIdentityOp2 Product = Axiom {
  description = "Multiplicative identity",
  example = (
    (Op2 Product (Op2 Product (Const 1) (Var "a")) (Const 1)),
    (Var "a")
  ),
  implementation = f
}
  where
    f (Op2 Product (Const 1) t) = Right t
    f (Op2 Product t (Const 1)) = Right t
    f (Op2 Fraction t (Const 1)) = Right t
    f t = Left t

axiomNullOp2 Exponent = Axiom {
  description = "Op2 Exponent of 0",
  example = (
    (Op2 Exponent (Var "a") (Const 0)),
    (Const 1)
  ),
  implementation = f
}
  where
    f (Op2 Exponent t (Const 0)) = Right (Const 1)
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
    (Op2 (Series "k") (Var "a") (Var "k")),
    (Op2 Sum (Var "a") (Op2 (Series "k") (Op2 Sum (Var "a") (Const 1)) (Var "k")))
  ),
  implementation = f
}
  where
    f (Op2 (Series v) i t) = Right $
      Op2 Sum
        (walk (instantiateVariable v i) t)
        (Op2 (Series v) (Op2 Sum i (Const 1)) t)
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
    f (Op2 Sum (Op2 Product p1l p1r) (Op2 Product p2l p2r))
      | p1l == p2l = Right $ Op2 Product p1l (Op2 Sum p1r p2r)
    f (Op2 Product pl (Op2 Sum sl sr)) = Right $ Op2 Sum (Op2 Product pl sl) (Op2 Product pl sr)
    f (Op2 Sum v@(Var{}) p@(Op2 Product _ _)) = f (Op2 Sum (Op2 Product v (Const 1)) p)
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
    f (Op1 Negate (Const a)) = Const (-a)
    f (Op1 Negate (Op1 Negate a)) = a
    f (Op2 Sum (Const a) (Const b)) = Const $ a + b
    f (Op2 Product (Const a) (Const b)) = Const $ a * b
    f (Op2 Exponent a (Const 0)) = Const 1
    f (Op2 Exponent a (Const 1)) = a
    f (Op2 Exponent (Const a) (Const b)) = Const $ a ^ b
    f (Op2 Fraction a (Const 1)) = a
    f (Op2 Fraction a (Const (-1))) = f $ Op1 Negate a
    f t@(Op2 Fraction (Const a) (Const b)) =
      case gcd a b of
        1 -> t
        n -> simplify $ Op2 Fraction (Const $ a `div` n) (Const $ b `div` n)
    f (Op2 Product (Const 1) a) = a
    f (Op2 Product a (Const 1)) = a
    f (Op2 Product (Const (-1)) a) = f $ Op1 Negate a
    f (Op2 Product a (Const (-1))) = f $ Op1 Negate a
    f (Op2 Sum a (Const 0)) = a
    f (Op2 Sum (Const 0) a) = a
    f x = x

distribute t (Op2 Product a (Op2 Sum b c)) =
  let x = cancelTerm t $ Op2 Fraction a t in

  Op2 Product x $
    Op2 Sum
      (Op2 Product t b)
      (Op2 Product t c)

undistribute t (Op2 Sum a b) =
  Op2 Product t $
    Op2 Sum
      (cancelTerm t $ Op2 Fraction a t)
      (cancelTerm t $ Op2 Fraction b t)

cancelTerm :: Term -> Term -> Term
cancelTerm (Op2 Exponent x y) f@(Op2 Fraction (Op2 Exponent a b) (Op2 Exponent c d)) =
  case Op2 Fraction <$> numerator <*> denominator of
    Just x -> x
    Nothing -> f
  where
    numerator = if x == a then Just (Op2 Exponent a (Op2 Sum b (Op2 Product (Const (-1)) y))) else Nothing
    denominator = if x == c then Just (Op2 Exponent c (Op2 Sum d (Op2 Product (Const (-1)) y))) else Nothing

cancelTerm t f@(Op2 Fraction (Op2 Exponent _ _) (Op2 Exponent _ _)) = cancelTerm (Op2 Exponent t (Const 1)) f
cancelTerm t (Op2 Fraction lhs@(Op2 Exponent _ _) rhs) = cancelTerm t (Op2 Fraction lhs (Op2 Exponent rhs (Const 1)))
cancelTerm t (Op2 Fraction lhs rhs@(Op2 Exponent _ _)) = cancelTerm t (Op2 Fraction (Op2 Exponent lhs (Const 1)) rhs)
cancelTerm t f@(Op2 Fraction (Op2 Product a b) (Op2 Product c d)) =
    case Op2 Fraction <$> numerator <*> denominator of
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
cancelTerm t (Op2 Fraction l@(Op2 Product _ _) r) = cancelTerm t (Op2 Fraction l (Op2 Product r (Const 1)))
cancelTerm t (Op2 Fraction l r@(Op2 Product _ _)) = cancelTerm t (Op2 Fraction l (Op2 Product (Const 1) r))
cancelTerm t (Op2 Fraction l r) = cancelTerm t (Op2 Fraction (Op2 Product (Const 1) l) (Op2 Product (Const 1) r))

goLeft :: Zipper -> Zipper
goLeft (Op2 op l r, cs) = (l, LeftCrumb (Op2 op Hole r):cs)
goLeft (t, cs) = (Hole, cs)

goRight :: Zipper -> Zipper
goRight (Op2 op l r, cs) = (r, RightCrumb (Op2 op l Hole):cs)
goRight (Op1 Factorial t, cs) = (t, RightCrumb (Op1 Factorial Hole):cs)
goRight (t, cs) = (Hole, cs)

goUp :: Zipper -> Zipper
goUp (t, LeftCrumb (Op2 op _ r):cs) = (Op2 op t r, cs)
goUp (t, RightCrumb (Op2 op l _):cs) = (Op2 op l t, cs)

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
termEqual (Op1 op1 a) (Op1 op2 c) = op1 == op2 && a `termEqual` c
termEqual (Op2 op1 a b) (Op2 op2 c d) = op1 == op2 && a `termEqual` c && b `termEqual` d
termEqual (Var a) (Var c) = a == c
termEqual (Const a) (Const c) = a == c
termEqual _ _ = False

instance IsString Term where
    fromString cs = parseUnsafe cs

walk :: (Term -> Term) -> Term -> Term
walk f (Op1 op t) = f (Op1 op (walk f t))
walk f (Op2 op a b) = f (Op2 op (walk f a) (walk f b))
walk f t@(Const{}) = f t
walk f t@(Var{}) = f t

precedence (Op1 Factorial _) = 40
precedence (Op2 Exponent _ _) = 30
precedence (Op2 Product _ _) = 20
precedence (Op2 Fraction _ _) = 20
precedence (Op2 Sum _ _) = 10
precedence _ = 99

maybeBrackets parent child = let inner = toUnicode child in
  if precedence child < precedence parent then
    "(" <> inner <> ")"
  else
    inner

toUnicode Hole             = "_"
toUnicode (Const a)        = show a
toUnicode (Var a)          = a
toUnicode t@(Op2 Sum a (Op1 Negate b))      = maybeBrackets t a <> " - " <> maybeBrackets t b
toUnicode t@(Op2 Sum a b) = maybeBrackets t a <> " + " <> maybeBrackets t b
toUnicode t@(Op2 Product a b)  =
  let operator = case (a, b) of
                   (_, Const{}) -> "⋅"
                   (_, Op1 Negate _) -> "⋅"
                   _            -> ""
  in maybeBrackets t a <> operator  <> maybeBrackets t b

toUnicode t@(Op1 Factorial a)  = maybeBrackets t a <> "!"
toUnicode t@(Op2 Fraction a b) = maybeBrackets t a <> "/" <> maybeBrackets t b
toUnicode t@(Op2 Exponent a b) = maybeBrackets t a <> "^" <> maybeBrackets t b
toUnicode t@(Op1 Negate a) = "-" <> maybeBrackets t a
toUnicode t@(Op1 (Function name) a) = name <> "(" <> toUnicode a <> ")"
toUnicode (Op2 (Series v) i t)   =
  "Σ[" <> v <> " = " <> toUnicode i <> "](" <> toUnicode t <> ")"
toUnicode (Op2 (Limit v) i t)   =
  "lim[" <> v <> " → " <> toUnicode i <> "](" <> toUnicode t <> ")"

-- TODO: Use -> for arrow
toAscii :: Term -> String
toAscii = replace 'Σ' 'S' . replace '⋅' '*' . replace '→' '>' . toUnicode

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

table = [ [postfix "!" (Op1 Factorial), series "S", limit "lim", functionExpr, prefix "-" (Op1 Negate) ]
        , [binary "^" (Op2 Exponent) AssocLeft ]
        , [binary "*" (Op2 Product) AssocLeft, binary "/" (Op2 Fraction) AssocLeft, binary "" (Op2 Product) AssocLeft]
        , [binary "+" (Op2 Sum) AssocLeft, binary "-" (\a b -> Op2 Sum a (Op1 Negate b)) AssocLeft ]
        ]

functionExpr :: Monad m => Operator String u m Term
functionExpr = Prefix . try $ do
  name <- msum . map string $ allowedFunctionNames
  return $ Op1 (Function name)

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

    return $ Op2 (Series v) i

limit op = Prefix $
  do
    string op
    char '['
    v <- replicate 1 <$> oneOf ['a'..'z']
    whiteSpace
    string "->"
    whiteSpace
    i <- expr
    char ']'

    return $ Op2 (Limit v) i

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
e_to t = (Op2 (Series "k") (Const 0) (Op2 Fraction (Op2 Exponent t (Var "k")) (Op1 Factorial (Var "k"))))
cos_x = parseUnsafe "S[m=0]((-1)^m*(x^(2*m))/(2*m)!)"

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
--  focus "_/h" $ apply axiomDistributeOp2 Product
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
    , ("sin(x)", "sin(x)")
    , ("S[h=0](h)", "S[h=0](h)")
    , ("lim[h->0](h)", "lim[h->0](h)")
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
    , testGroup "random"
      [ testCase "functions not parsed as products of variables" $
          Left "sin(x)" @=? (implementation axiomCommuteProduct) "sin(x)"
      ]
  ]

runApp :: Env -> Eff '[ Writer Log, State Env] a -> (Term, Log)
runApp env m = do
  let ((_, log), (Env t)) = run . runState env . runWriter $ m

  (t, log)
