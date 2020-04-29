module CLex where
  
import Data.Char
import Control.Monad
import ParserCombinator
import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>), sequence)



--- LEX SCANNER ---
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexIgnore *> greedy (lexToken <* lexIgnore) <* eof



--- ALL TOKENS ---
data Token  = POpen | PClose      
            | SOpen | SClose      
            | COpen | CClose      
            | Comma    
            | Semicolon
            | KeyIf    | KeyElse
            | KeyWhile | KeyFor 
            | KeyReturn
            | KeyClass | KeyVoid
            | CType     String
            | Operator  String    
            | UpperId   String  
            | LowerId   String  
            | ValueInt  Int
            | ValueBool Bool
            | ValueChar Char
            deriving (Eq, Show)



--- ALL TERMINALS ---
terminals :: [(Token, String)]
terminals =
    [ ( POpen     , "("      )
    , ( PClose    , ")"      )
    , ( SOpen     , "["      )
    , ( SClose    , "]"      )
    , ( COpen     , "{"      )
    , ( CClose    , "}"      )
    , ( Comma     , ","      )
    , ( Semicolon , ";"      )
    , ( KeyIf     , "if"     )
    , ( KeyElse   , "else"   )
    , ( KeyWhile  , "while"  )
    , ( KeyFor    , "for"    )
    , ( KeyReturn , "return" )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    ]



--- ALL TYPES ---
ctypes :: [String]
ctypes = ["int", "long", "double", "float", "bool", "char"]



--- ALL OPERATORS ---
operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "<=", "<", ">=", ">", "==", "!=", "="]



--- LEX STUFF --
lexIgnore :: Parser Char String
lexIgnore = lexWhiteSpace <* many (lexSingleComment <* lexWhiteSpace)

lexSingleComment :: Parser Char String
lexSingleComment = token "/*" *> greedy (satisfy (/= '\n')) <* (() <$ symbol '\n' <|> () <$ eof)

lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexValueInt :: Parser Char Token
lexValueInt = ValueInt . read <$> greedyWithoutSucceed (satisfy isDigit)

lexValueChar :: Parser Char Token
lexValueChar = ValueChar <$ symbol '\'' <*> anySymbol <* symbol '\''

lexValueBool :: Parser Char Token
lexValueBool = ValueBool True <$ token "true" <|> ValueBool False <$ token "false"

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]



--- SATISFY FUNCTIONS ---
sCTypes :: Parser Token Token
sCTypes = satisfy isCTypes
    where isCTypes (CType _) = True
          isCTypes _         = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
    where isLowerId (LowerId _) = True
          isLowerId _           = False

sValue :: Parser Token Token
sValue  = satisfy isValue
    where isValue (ValueInt  _) = True
          isValue (ValueChar _) = True
          isValue (ValueBool _) = True
          isValue _             = False

sOperator :: Parser Token Token
sOperator = satisfy isOperator
    where isOperator (Operator _) = True
          isOperator _            = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon



--- KEYWORD ---
keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs