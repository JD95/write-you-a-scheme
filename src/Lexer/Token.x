{
module Lexer.Token where
import Data.Monoid
import Data.String.Conv
import qualified Text.Read as R
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ ;
  "--".* ;


  define                        { ignoreInput TokenDefine }
  lambda                        { ignoreInput TokenLambda }
  $digit+                       { usingInput TokenInt }
  \(                            { ignoreInput TokenLParen }
  \)                            { ignoreInput TokenRParen }
  $alpha [$alpha $digit \_ \']* { usingInput' TokenSym }
  \'                            { ignoreInput TokenQuote }
  \#t                           { ignoreInput (TokenBool True)}
  \#f                           { ignoreInput (TokenBool False)}
{

usingInput f (p,_,s,_) l = do
  case (R.readMaybe . toS . ByteString.take l $ s) of 
      Just i -> pure $ L p (f i)
      Nothing -> alexError $ "Couldn't parse from " ++ toS s
  
usingInput' f (p,_,s,_) l = pure $ L p (f (ByteString.take l s))
ignoreInput f (p,_,s,_) l = pure $ L p f

alexEOF = pure EOF
lexwrap = (alexMonadScan >>=)

parseError :: Lexeme -> Alex a	
parseError _ = alexError "Why is using happy and alex so hard"

scanner str = runAlex

data Lexeme = L AlexPosn Token | EOF deriving (Show)

-- The token type:
data Token = TokenInt Int
           | TokenSym ByteString.ByteString
           | TokenLParen
           | TokenRParen
	   | TokenQuote
	   | TokenDefine
	   | TokenLambda
	   | TokenBool Bool 
           deriving (Eq,Show)

}
