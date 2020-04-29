module CParser where
import CLex
import ParserCombinator
import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprMethod Token [Expr]
          deriving Show

data Decl = Decl Type Token
          deriving Show
      
data Type = TypeVoid
          | TypePrim  Token
          | TypeArray Type
          deriving (Eq,Show)

-- Data type for operator associativity.
data Assoc = LR | RL deriving (Eq)

parenthesised p = symbol POpen *> p <* symbol PClose
bracketed     p = symbol SOpen *> p <* symbol SClose
braced        p = symbol COpen *> p <* symbol CClose

-- List of operators, their presedences and whether they're associated to the left or right. [(Operator, Presedence, Assoc)]
opAssoc :: [(Token, Int, Assoc)]
opAssoc = [
    (Operator "*",  1, LR),
    (Operator "/",  1, LR),
    (Operator "%",  1, LR),
    (Operator "+",  2, LR),
    (Operator "-",  2, LR),
    (Operator "<",  3, LR),
    (Operator "<=", 3, LR),
    (Operator ">",  3, LR),
    (Operator ">=", 3, LR),
    (Operator "==", 4, LR),
    (Operator "!=", 4, LR),
    (Operator "&&", 6, LR),
    (Operator "||", 7, LR),
    (Operator "=",  9, RL)
  ]

-- Compares the precedence of two operators. 
-- Returns True when the first operator has a higher presendence than the second, otherwise False.
-- This function also handles associativity.
compPrecedence :: Token -> Token -> Bool
compPrecedence op1 op2 = let (prec1, asoc1) = getPrec op1 opAssoc
                             (prec2, asoc2) = getPrec op2 opAssoc
                         in prec1 < prec2 || (op1 == op2 && asoc1 == LR)
  where
    getPrec :: Token -> [(Token, Int, Assoc)] -> (Int, Assoc)
    getPrec _ []  = (8, LR)
    getPrec op@(Operator o) ((Operator x, i, a):xs)
      | o == x    = (i, a)
      | otherwise = getPrec op xs

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr   <$> pExpr <* sSemi
     <|> StatIf     <$  symbol KeyIf     <*> parenthesised pExpr    <*> pStat <*> optionalElse
     <|> StatWhile  <$  symbol KeyWhile  <*> parenthesised pExpr    <*> pStat
     <|> forToWhile <$  symbol KeyFor    <*  symbol POpen <*> pExpr <*  sSemi <*> pExpr <* sSemi <*> pExpr <* symbol PClose <*> pStat
     <|> StatReturn <$  symbol KeyReturn <*> pExpr                  <*  sSemi
     <|> pBlock
     where
        optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])

        -- Desugar the for loop into a while loop.
        forToWhile :: Expr -> Expr -> Expr -> Stat -> Stat
        forToWhile init expr increment forBody = StatBlock (StatExpr init : [StatWhile expr (StatBlock (forBody : [StatExpr increment]))])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi
      