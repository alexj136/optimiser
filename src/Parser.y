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
    newline { ( TK_Newline    , _ ) }
    add     { ( TK_Add        , _ ) }
    sub     { ( TK_Sub        , _ ) }
    mul     { ( TK_Mul        , _ ) }
    colon   { ( TK_Colon      , _ ) }
    assign  { ( TK_Assign     , _ ) }
    if      { ( TK_If         , _ ) }
    goto    { ( TK_Goto       , _ ) }
    else    { ( TK_Else       , _ ) }
    greater { ( TK_Greater    , _ ) }
    zero    { ( TK_Zero       , _ ) }
    int     { ( TK_Int     $$ , _ ) }
    name    { ( TK_Name    $$ , _ ) }
%%

PROG :: { [LinearStatement] }

PROG
    : STMT newline PROG                       { $1 : $3                      }
    | newline PROG                            { $2                           }
    | STMT                                    { [$1]                         }
    | {- empty -}                             { []                           }

STMT :: { LinearStatement   }
STMT
    : name colon                              { Label $1                     }
    | if VAL greater zero goto name else name { If $2 $6 $8                  }
    | goto name                               { Goto $2                      }
    | name assign VAL                         { Assign (FromOne $1 $3)       }
    | name assign VAL OP VAL                  { Assign (FromTwo $1 $3 $4 $5) }

VAL  :: { Val               }
VAL
    : name                                    { Var $1                       }
    | int                                     { Lit $1                       }
    | zero                                    { Lit 0                        }

OP   :: { Op                }
OP
    : add                                     { Add                          }
    | sub                                     { Sub                          }
    | mul                                     { Mul                          }
{
parseError :: [Token] -> a
parseError []                = error "Reached end of file while parsing"
parseError ((tk,(y,x)) : ts) = error $ concat ["Parse error on line ", show y,
                               ", column ", show x,"."]
}
