module ParserCombinator where

import Prelude hiding ((>>=), (<$), (<*), (*>), (<*>), (<$>))


--- PARSER TYPE ---
type Parser s r = [s] -> [(r,[s])] 


--- PARSER OPERATORS ---

-- INFIXL --
infixl 4 <$
infixl 4 <*
infixl 4 *>
infixl 4 <$>
infixl 4 <*>
infixr 3 <|>
infixr 3 <<|>
infixl 1 >>=

-- OPERATORS --
(<*>) :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) xs  =  [(f x,zs) |(f,ys) <- p xs,(x,zs) <- q ys] -- Parser compatible version of <*> based of the prelude implementation

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p <|> q) xs  =  p xs ++ q xs -- Parser compatible version of <|> based of the prelude implementation

(>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
(p >>= f) xs  =  [(z  ,zs)|(y  ,ys) <- p xs,(z  ,zs) <- f y ys] -- ParserCombinator compatible version of >>= based of the prelude implementation

(<$) :: b -> Parser s a -> Parser s b
f <$ p = const f <$> p  -- ParserCombinator compatible version of <$ based of the prelude implementation

(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = const <$> p <*> q  -- ParserCombinator compatible version of <* based of the prelude implementation

(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = flip const <$> p <*> q  -- ParserCombinator compatible version of *> based of the prelude implementation



--- BASIC PARSERS ---
succeed :: a -> Parser s a
succeed r xs = [(r,xs)] --Always succeeds

fail :: Parser s a
fail xs  =  [] --Always fails

epsilon :: Parser s ()
epsilon = succeed () -- Returns ()

satisfy  ::  (s -> Bool) -> Parser s s
satisfy p (x:xs) | p x       =  [(x,xs)] -- Satisfy predicate p
satisfy _ _                  =  []

symbol :: Eq s  => s -> Parser s s
symbol x = satisfy (==x) -- Parses symbol x

anySymbol :: Parser s s
anySymbol (x:xs)  =  [(x,xs)]
anySymbol []      =  []

some :: Parser s a -> Parser s [a]
some p = (:) <$> p <*> many p -- Parses some p (1 or more)

many :: Parser s a  -> Parser s [a]
many p  =  (:) <$> p <*> many p <|> succeed [] -- Parses many p (0 or more)

greedy :: Parser s a -> Parser s [a]
greedy p  =  (:) <$> p <*> greedy p <<|> succeed [] -- Greedy Parses p ()

greedyWithoutSucceed :: Parser s b -> Parser s [b] -- Greedy parser that fails if no characters are parsed
greedyWithoutSucceed p = (:) <$> p <*> greedy p

token :: Eq s => [s] -> Parser s [s]
token = foldr (\x -> (<*>) ((:) <$> symbol x)) (succeed [])

parse :: Parser s a -> [s] -> [(a, [s])]
parse = id -- runs the parser

look :: Parser s [s]
look xs = [(xs, xs)] -- Looks ahead without consuming anything, used in eof parser

eof :: Parser s ()
eof = look >>= \xs -> if null xs then epsilon else fail -- parses eof