import Control.Applicative
import Data.Char

data Expr = Num Double | Add Expr Expr
  deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)
    
instance Applicative Parser where
  pure x = Parser $ \input -> Just(x, input)
  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (f, rest1) <- p1 input
    (x, rest2) <- p2 rest1
    Just (f x, rest2)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \input -> case p1 input of
    Nothing -> p2 input
    res -> res
    
instance Monad Parser where
  Parser p >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest
    
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \input -> case input of
  (c:cs) | p c -> Just(c, cs)
  _            -> Nothing
  
  
char :: Char -> Parser Char
char c = satisfy (== c)

ws :: Parser String
ws = many $ satisfy isSpace

double :: Parser Double
double = Parser $ \input ->
  let (digits, rest) = span isDigit input
  in if null digits
  then Nothing
  else Just (read digits, rest)

num :: Parser Expr
num = Num <$> double

term :: Parser Expr
term = ws *> (num <|> parenthesized) <* ws
  where parenthesized = char '(' *> ws *> expr <* ws <* char ')'
  
expr :: Parser Expr
expr = do
  t <- term
  ts <- many (ws *> char '+' *> ws *> term)
  return (foldl Add t ts)

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
  Just (result, "") -> Just result
  _ -> Nothing
  
eval :: Expr -> Double
eval (Num x) = x
eval (Add e1 e2) = eval e1 + eval e2

main :: IO ()
main = do
  let foo = parse expr "1 + 2 + (4 + 5)"
  let bar = parse expr "10 + 20"
  
  case foo of
    Just e -> print $ eval e
    _ -> print "Error parsing"
    
  case bar of
    Just e -> print $ eval e
    _ -> print "Error parsing"
  
