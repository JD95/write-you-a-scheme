{

module Parser.Grammar (Exp_ (..), parseExp, Exp) where
  
import Prelude hiding (GT, LT, EQ)
import Lexer.Token
import Data.Functor.Foldable
import Parser.AST    
import Data.List.NonEmpty (NonEmpty(..), cons, toList)  
import Data.Semigroup  
}

%name parseGrammar
%lexer {lexwrap} {EOF}
%monad {Alex}
%error { parseError }
%tokentype { Lexeme }

%token
int    { L _ (TokenInt $$) }
sym    { L _ (TokenSym $$) }
bool   { L _ (TokenBool $$) }
'('    { L _ TokenLParen }
')'    { L _ TokenRParen }
"'"    { L _ TokenQuote }
define { L _ TokenDefine }
lambda { L _ TokenLambda }
%%

Exp : Constant                           { $1 }
    | Symbol                             { $1 }
    | "'" Exp                            { Fix (Quote $2) }
    | '(' define '(' Symbols')' Exp ')'  { Fix (Define $4 $6) }
    | '(' lambda '(' Symbols ')' Exp ')' { Fix (Lambda $4 $6) }
    | '(' List ')'                       { Fix (List (toList $2)) }

Constant : int  { Fix (I $1) }
         | bool { Fix (B $1) }

Symbols : Symbols Symbol { $1 <> ($2 :| []) }
        | Symbol      { $1 :| [] }

Symbol : sym { Fix (Sym $1) }

List : List Exp { $1 <> ($2 :| [])}
     | Exp      { $1 :| [] }

{
  parseExp str = runAlex str parseGrammar
}
