{
module Parser where

import Lexer
import Syntax
}

%name parse     PROG
%name parseStmt STMT

%tokentype { Token      }
%error     { parseError }

%token
    newline { ( TK_Newline    , p ) }
    add     { ( TK_Add        , p ) }
    sub     { ( TK_Sub        , p ) }
    mul     { ( TK_Mul        , p ) }
    colon   { ( TK_Colon      , p ) }
    assign  { ( TK_Assign     , p ) }
    if      { ( TK_If         , p ) }
    goto    { ( TK_Goto       , p ) }
    else    { ( TK_Else       , p ) }
    greater { ( TK_Greater    , p ) }
    zero    { ( TK_Zero       , p ) }
    int     { ( TK_Int     $$ , p ) }
    name    { ( TK_Name    $$ , p ) }
%%

PROG :: { [LinearStatement] }

PROG
    : STMT newline PROG                      { $1 : $3                       }
    | newline PROG                           { $2                            }
    | STMT                                   { [$1]                          }
    | {- empty -}                            { []                            }

STMT :: { LinearStatement   }
STMT
    : name colon                             { Label $1                      }
    | if name greater zero goto VAL else VAL { If $2 $6 $8                   }
    | goto name                              { Goto $2                       }
    | name assign VAL                        { Assign (FromOne $1 $3)        }
    | name assign VAL OP VAL                 { Assign (FromTwo $1 $3 $4 $5 ) }

VAL  :: { Val               }
VAL
    : name                                   { Var $1                        }
    | int                                    { Lit $1                        }
    | zero                                   { Lit 0                         }

OP   :: { Op                }
OP
    : add                                    { Add                           }
    | sub                                    { Sub                           }
    | mul                                    { Mul                           }
{
parseError :: [Token] -> a
parseError []     = error "Reached end of file while parsing"
parseError (t:ts) = error ("Parse error on line " ++ show (getY t) ++
                           ", column " ++ show (getX t) ++ ".")
}
