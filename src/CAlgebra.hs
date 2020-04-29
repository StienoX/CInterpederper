module CAlgebra where

import CLex
import CParser

type CAlgebra stat expr
    = ( 
        (  Decl                  -> stat
         , expr                  -> stat
         , expr -> stat -> stat  -> stat
         , expr -> stat          -> stat
         , expr                  -> stat
         , [stat]                -> stat
         )
        ,
        (  Token                  -> expr
         , Token                  -> expr
         , Token -> expr -> expr  -> expr
         , Token -> [expr]        -> expr
         )
      )
      

foldC :: CAlgebra stat expr -> Class -> clas
foldC (c1, (m1,m2), (s1,s2,s3,s4,s5,s6), (e1,e2,e3,e4)) = fClas
    where
        fStat (StatDecl   d)        = s1 d
        fStat (StatExpr   e)        = s2 (fExpr e)
        fStat (StatIf     e s1 s2)  = s3 (fExpr e) (fStat s1) (fStat s2)
        fStat (StatWhile  e s1)     = s4 (fExpr e) (fStat s1)
        fStat (StatReturn e)        = s5 (fExpr e)
        fStat (StatBlock  ss)       = s6 (map fStat ss)
        fExpr (ExprConst  con)      = e1 con
        fExpr (ExprVar    var)      = e2 var
        fExpr (ExprOper   op e1 e2) = e3 op (fExpr e1) (fExpr e2)
        fExpr (ExprMethod s xs)     = e4 s (map fExpr xs)

