module Parser where


import            Control.Applicative             ((<|>), Alternative (..), optional)
import            Control.Monad.Combinators.Expr  (makeExprParser, Operator (..))
import            Control.Comonad.Cofree          (Cofree ((:<)))
import            Data.Functor                    (($>), void)
import            Data.Void                       (Void)
import            Data.Composition                ((.:))
import qualified  Data.Text       as T
import            Text.Megaparsec                 (Parsec, getSourcePos, between, sepBy, oneOf, choice, manyTill, anySingle, MonadParsec (try, lookAhead, eof), ParseErrorBundle, parse)
import            Text.Megaparsec.Char.Lexer      (lexeme)
import            Text.Megaparsec.Char            (space, hspace, string, letterChar, digitChar, char, newline)
import            Source                          (SrcSpan, srcSpan, Source (..))
import            Syntax                          (Expr, ExprF(..), Stmt (..), VarDef (..), FunDef (..), Param (..), Ast (..), Toplevel (..))

type Parser = Parsec Void T.Text

l :: Parser a -> Parser a
l = lexeme space

lh :: Parser a -> Parser a
lh = lexeme hspace

lstring :: T.Text -> Parser T.Text
lstring = l . string

lstring_ :: T.Text -> Parser ()
lstring_ = void . lstring 

parens :: Parser a -> Parser a
parens = between (lstring "(") (string ")")

bracks :: Parser a -> Parser a
bracks = between (lstring "[") (string "]")

curlybracks :: Parser a -> Parser a
curlybracks = between (lstring "{") (string "}")

name' :: Parser T.Text
name' =
    T.cons <$> head' <*> (T.pack <$> many tail')
  where
    head' = letterChar <|> char '_'
    tail' = head' <|> digitChar <|> oneOf ['\'', '-']

src :: Parser a -> Parser (SrcSpan, a)
src p = do
  begin <- getSourcePos
  a <- p
  end <- getSourcePos
  pure (srcSpan begin end, a)

cosrc :: Parser (f (Cofree f SrcSpan)) -> Parser (Cofree f SrcSpan)
cosrc = (uncurry (:<) <$>) . src

cosrc1
  :: Parser (Cofree f SrcSpan -> f (Cofree f SrcSpan))
  -> Parser (Cofree f SrcSpan -> Cofree f SrcSpan)
cosrc1 p = do
  (s, f) <- src p
  pure $ (s :<) . f

cosrc2
  :: Parser (Cofree f SrcSpan -> Cofree f SrcSpan -> f (Cofree f SrcSpan))
  -> Parser (Cofree f SrcSpan -> Cofree f SrcSpan -> Cofree f SrcSpan)
cosrc2 p = do
  (s, f) <- src p
  pure $ (s :<) .: f

dynamict :: Parser (Expr SrcSpan)
dynamict = cosrc $ string "?" $> DynamicT

arrayt :: Parser (Expr SrcSpan)
arrayt = cosrc $ ArrayT <$> bracks texpr

arrayl :: Parser (Expr SrcSpan)
arrayl = cosrc $ ArrayL <$> bracks (sepBy expr $ lstring ",")

recordt :: Parser (Expr SrcSpan)
recordt =
    cosrc $ RecordT <$> curlybracks (sepBy field $ lstring ",")
  where
    field = (,) <$> l name' <*> (lstring ":" *> l texpr)

recordl :: Parser (Expr SrcSpan)
recordl =
    cosrc $ RecordL <$> curlybracks (sepBy field $ lstring ",")
  where
    field = (,) <$> l name' <*> (lstring "=" *> l expr)

stringl :: Parser (Expr SrcSpan)
stringl =
  cosrc . fmap (StringL . T.pack) $ char '"' *> manyTill anySingle (char '"')

name :: Parser (Expr SrcSpan)
name = cosrc $ Name <$> name'

unitt :: Parser (Expr SrcSpan)
unitt = cosrc $ string "{}" $> UnitT

unitl :: Parser (Expr SrcSpan)
unitl = cosrc $ string "{}" $> UnitL

texpr :: Parser (Expr SrcSpan)
texpr = choice [try unitt, arrayt, recordt, name, dynamict]

expr :: Parser (Expr SrcSpan)
expr =
    makeExprParser term table
  where
    term = choice
      [ try unitl
      , arrayl
      , recordl
      , stringl
      , name
      , parens expr
      ]
    table =
      -- TODO: Infix operators
      [ [ invoke' ]
      , [ select' ]
      ]
    invoke' = 
      Postfix . cosrc1 $ flip Invoke <$> parens (sepBy expr $ lstring ",")
    select' = 
      Postfix . cosrc1 $ flip Select <$> (string "." *> name')

varDef :: Parser (VarDef SrcSpan)
varDef = do
  lstring_ "local"
  varDefName  <- l name'
  varDefType  <- optional $ lstring ":" *> l texpr
  varDefValue <- lstring "=" *> expr
  pure MkVarDef {..}

stmt :: Parser (Stmt SrcSpan)
stmt =
    choice [var', return', stmt']
  where
    var' = LocalDef <$> varDef
    return' = fmap Return $ lstring "return" *> expr
    stmt' = Stmt <$> expr

stmts :: Parser [Stmt SrcSpan]
stmts = 
    manyTill (lh stmt <* lineTerm) stmtsTerm
  where
    lineTerm = void (l newline) <|> void (lstring ";")
    stmtsTerm = lookAhead $ string "end"

param :: Parser (Param SrcSpan)
param = do
  (paramNameSpan, paramName)  <- l $ src name'
  paramType                   <- lstring ":" *> l texpr
  pure MkParam {..}
    
params :: Parser [Param SrcSpan]
params = parens . sepBy param $ lstring ","

funDef :: Parser (FunDef SrcSpan)
funDef = do
  lstring_ "function"
  funDefName    <- l name'
  funDefParams  <- l params
  funDefRetType <- optional $ lstring ":" *> l texpr
  funDefBody    <- l stmts <* lstring "end"
  pure MkFunDef {..}

-- | Attempts to parse an 'Ast' from a given source file.
parseAst :: Source -> Either (ParseErrorBundle T.Text Void) (Ast SrcSpan)
parseAst src'@MkSource {..} =
    MkAst src' <$> parse (many def <* eof) sourcePath sourceContent
  where
    def = l $ choice [VarDef <$> varDef, FunDef <$> funDef]