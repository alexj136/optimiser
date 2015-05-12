{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+                ;
    \n|\;.*\n              { \p s -> ( TK_Newline      , pos p ) }
    "+"                    { \p s -> ( TK_Add          , pos p ) }
    "-"                    { \p s -> ( TK_Sub          , pos p ) }
    "*"                    { \p s -> ( TK_Mul          , pos p ) }
    ":"                    { \p s -> ( TK_Colon        , pos p ) }
    ":="                   { \p s -> ( TK_Assign       , pos p ) }
    "if"                   { \p s -> ( TK_If           , pos p ) }
    "goto"                 { \p s -> ( TK_Goto         , pos p ) }
    "else"                 { \p s -> ( TK_Else         , pos p ) }
    ">"                    { \p s -> ( TK_Greater      , pos p ) }
    "0"                    { \p s -> ( TK_Zero         , pos p ) }
    [1-9][0-9]*            { \p s -> ( TK_Int (read s) , pos p ) }
    $alpha [$alnum \_]*    { \p s -> ( TK_Name s       , pos p ) }

{
type Token = (TokenKind, TokenPos)

data TokenKind
    | TK_Newline
    | TK_Add
    | TK_Sub
    | TK_Mul
    | TK_Colon
    | TK_Assign
    | TK_If
    | TK_Goto
    | TK_Else
    | TK_Greater
    | TK_Zero
    | TK_Int Int
    | TK_Name String
    deriving (Show, Eq, Ord)

getX :: Token -> Int
getX (_, (_, x)) = x
getY :: Token -> Int
getX (_, (y, _)) = y

-- The lexer function
scan :: String -> [Token]
scan = alexScanTokens

-- Extract token coordinates from AlexPosn object
pos :: AlexPosn -> (Int, Int)
pos (AlexPn i j k) = (j, k)
}
