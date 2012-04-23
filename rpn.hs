module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import Data.List
import Data.Functor.Identity
import Control.Monad
import Numeric
import Text.Parsec as P
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language (haskellStyle)



-- | Expression data types
-- (ConstStr String may serve as a storage for an infinite string representation of numbers.)

data Expr = ConstInt Integer | ConstFloat Double | ConstStr String | Neg Expr | Binary Op Expr Expr
data Op = Add | Sub | Mult | Div

-- | Shows expressions in RPN format
instance Show Expr where
  show (ConstInt a)      = show a
  show (ConstFloat a)    = show a
  show (ConstStr a)      = a
  show (Neg f)           = '_' : show f
  show (Binary op f g)   = show f ++ " " ++ show g ++ " " ++ show op
  
instance Show Op where
  show Add  = "+"
  show Sub  = "-"
  show Mult = "*"
  show Div  = "/"



-- | Main
main :: IO ()
main = do
  (args, expression) <- getArgs >>= parseArgs
  
  if Eval `elem` args
    then calc args expression
    else printConv $ convert expression

-- | Converts an expression in infix form into RPN
convert :: String -> Either String String
convert input   = case parseExpr BuiltForConversion input of
                  Left err -> Left $ show err
                  Right x  -> Right $ show x

-- | Prints out converted expression to stdout
printConv :: Either String String -> IO ()
printConv ex    = case ex of
                  Left err -> do hPutStr stderr "parse error at "
                                 hPutStr stderr err
                  Right x  -> putStrLn x

-- | Parses string into expression and evaluates result
calc :: Flags -> String -> IO ()
calc args input = case parseExpr BuiltForCalculations input of
                  Left err -> do hPutStrLn stderr "parse error at "
                                 hPrint stderr err
                  Right x  -> putStrLn $ showResult precision (eval x) "" where
                    showResult = if Scientific `elem` args then showEFloat else showFFloat
                    precision  = case determinePrecision args of
                                 Just value -> Just (read value)
                                 Nothing    -> Nothing



-- | Parses input
parseExpr :: ExprType -> String -> Either ParseError Expr
parseExpr exprType = parse parser "" where
  parser   = do whiteSpace
                x <- expr exprType
                eof
                return x



-- | Token parser
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser haskellStyle
        { PT.reservedOpNames = ["*","/","+","-","^"] }

whiteSpace      :: ParsecT String () Identity ()
whiteSpace      = PT.whiteSpace lexer
lexeme          :: forall a. ParsecT String () Identity a -> ParsecT String () Identity a
lexeme          = PT.lexeme lexer
parens          :: forall a. ParsecT String () Identity a -> ParsecT String () Identity a
parens          = PT.parens lexer
reservedOp      :: String -> ParsecT String () Identity ()
reservedOp      = PT.reservedOp lexer
naturalOrFloat  :: ParsecT String () Identity (Either Integer Double)
naturalOrFloat  = PT.naturalOrFloat lexer
dot             :: ParsecT String () Identity String
dot             = PT.dot lexer



-- | Expression parser
data ExprType = BuiltForCalculations | BuiltForConversion

expr      :: ExprType -> Parser Expr
expr exprType = buildExpressionParser table factor <?> "expression" where
  factor = case exprType of
           BuiltForCalculations -> factorForCalculations
           BuiltForConversion   -> factorForConversion

table     :: [[Operator String () Identity Expr]]
table     = [[op_prefix "-" Neg]
            ,[op "*" (Binary Mult) AssocLeft, op "/" (Binary Div) AssocLeft]
            ,[op "+" (Binary Add)  AssocLeft, op "-" (Binary Sub) AssocLeft]
            ]          
          where
            op' prefixOrInfix s f = prefixOrInfix (do{ reservedOp s; return f} <?> "operator")
            op          = op' Infix
            op_prefix   = op' Prefix

factorForCalculations :: ParsecT String () Identity Expr
factorForCalculations = parens (expr BuiltForCalculations)
          <|> do nf <- naturalOrFloat -- Stores numbers in Haskell number data types
                 case nf of
                   Left int    -> return (ConstInt int)
                   Right float -> return (ConstFloat float)
          <?> "number"

factorForConversion :: ParsecT String () Identity Expr
factorForConversion = parens (expr BuiltForConversion)
          <|> P.try (liftM ConstStr stringFloat)
          <|> liftM ConstStr stringDecimal
          <?> "number"

stringFloat :: ParsecT String () Identity String
stringFloat   = lexeme (do nat    <- many digit
                           radix  <- dot
                           frac   <- many1 digit
                           return (nat ++ radix ++ frac)
                       <?> "float")

stringDecimal :: ParsecT String () Identity String
stringDecimal = lexeme (many digit <?> "decimal")
                       
                       
        
-- | Evaluates expression
eval :: Expr -> Double
eval (ConstInt a)       = fromIntegral a
eval (ConstFloat a)     = a
eval (ConstStr a)       = read a
eval (Neg ex)           = negate (eval ex)
eval (Binary op f g)    = eval' (operator op) f g

eval' :: (Double -> Double -> Double) -> Expr -> Expr -> Double
eval' op a b            = op (eval a) (eval b)

operator :: forall a. Fractional a => Op -> a -> a -> a
operator Add  = (+) 
operator Sub  = (-) 
operator Mult = (*) 
operator Div  = (/) 



-- | Argument flags
type Flags = [Flag]
data Flag
  = Precision String
  | Scientific
  | Eval 
  | Version 
  | Help
  deriving (Eq, Show)

flags :: [OptDescr Flag]
flags =
  [Option "p"  []            (ReqArg Precision "precision")
      "Number of digits shown after the decimal point."
  ,Option "s"  []            (NoArg Scientific) 
      "Output in scientific format (e.g. 2.45e2, 1.5e-3)."
  ,Option "e"  []            (NoArg Eval) 
      "Evaluate expression (instead of conversion)."
  ,Option "V"  ["version"]   (NoArg Version)
      "Print version and exit..."
  ,Option "h?" ["help"] (NoArg Help)
      "Prints this help message."
  ]



-- | Parses arguments
--
-- Sources:
-- http://www.haskell.org/haskellwiki/Tutorials/Programming_Haskell/Argument_handling#Parsing_the_flags
-- http://cvs.haskell.org/Hugs/pages/libraries/base/System-Console-GetOpt.html
parseArgs :: [String] -> IO (Flags, String)
parseArgs argv = case getOpt Permute flags argv of
  (args,ex,[]) -> do
    let expression = if null ex then "" else concat ex
    let precision = case determinePrecision args of
                      Just value -> read value
                      Nothing    -> 1 -- anything that passes validation
                      
    parseArgs' args flags expression header precision

  (_,_,errs)   -> do
    hPutStrLn stderr (concat errs ++ usageInfo header flags)
    exitWith (ExitFailure 1)

  where header = "Usage: rpn [-p precision] [-ehsV] [--version] [--help] [\"<expression>\"].\n\
                  \Don't forget to use quote marks and parentheses."

parseArgs' :: Flags -> [OptDescr a] -> t -> String -> Integer -> IO (Flags, t)
parseArgs' args descriptors expression header precision
  | Help `elem` args      = do  putStr (usageInfo header descriptors)
                                exitSuccess
  | Version `elem` args   = do  putStrLn version
                                exitSuccess
  | precision < 0         = do  hPutStrLn stderr "Invalid precision, must be greater or equal to 0."
                                exitWith (ExitFailure 1)
  | otherwise             = return (nub (concatMap set args), expression)

set :: t -> [t]
set f = [f]

determinePrecision :: Flags -> Maybe String
determinePrecision [] = Nothing
determinePrecision (Precision s:_) = Just s
determinePrecision (_:rest) = determinePrecision rest



-- | Version info 
version :: String
version = "GNU rpn 1.0\n\
          \Copyright (C) 2007 Free Software Foundation, Inc.\n\
          \Complain about bugs & issues on https://github.com/mbixby/RPN.hs\n\
          \License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
          \This is free software: you are free to change and redistribute it.\n\
          \There is NO WARRANTY, to the extent permitted by law."


