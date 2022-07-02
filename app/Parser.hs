module Parser
  ( parseAst
  ) where

import           Control.Applicative              (Alternative(..))
import           Data.Bifunctor                   (Bifunctor(..))
import           Data.Void                        (Void)
import           Data.Functor                     (($>), void)
import qualified Data.Text                  as T
import           Text.Megaparsec                  (parse, Parsec, ParseErrorBundle, oneOf, getSourcePos, MonadParsec(..), choice, sepBy, between, manyTill, anySingle, option, sepBy1)
import           Text.Megaparsec.Char.Lexer       (lexeme)
import           Text.Megaparsec.Char             (space, string, letterChar, char, digitChar, hspace, newline)
import           Source                           (Source(..), SrcSpan, srcSpan)
import           Syntax                           (Ast(..), FunDef(..), Def(..), Stmt(..), Expr(..), Param(..), Type(..), LocalDef(..), RecordType(..), RecordLabel(..), RecordValue(..), RecordField(..), RecordSelect(..))

type Parser = Parsec Void T.Text

l :: Parser a -> Parser a
l = lexeme space

string_ :: T.Text -> Parser ()
string_ = void . string

lstring :: T.Text -> Parser T.Text
lstring = l . string

lstring_ :: T.Text -> Parser ()
lstring_ = void . lstring

lh :: Parser a -> Parser a
lh = lexeme hspace

src :: Parser a -> Parser (SrcSpan, a)
src p = do
  begin <- getSourcePos
  a <- p
  end <- getSourcePos
  pure (srcSpan begin end, a)

lsrc :: Parser a -> Parser (SrcSpan, a)
lsrc = l . src

name :: Parser T.Text
name = T.cons <$> head' <*> (T.pack <$> many tail')
  where
    head' = letterChar <|> char '_'
    tail' = head' <|> digitChar <|> oneOf ['\'', '-']

parens :: Parser a -> Parser a
parens = between (lstring "(") (string ")")

recordLabel :: Parser RecordLabel
recordLabel = 
    choice [dyn, typed]
  where
    dyn = do
      lstring_ "?"
      (recordLabelNameSpan, recordLabelName) <- src name
      let recordLabelTypeSpan = Nothing
          recordLabelType     = TDynamic
      pure MkRecordLabel {..}
    typed = do
      (recordLabelNameSpan, recordLabelName) <- lsrc name
      lstring_ ":"
      (recordLabelTypeSpan', recordLabelType) <- src type'
      let recordLabelTypeSpan = Just recordLabelTypeSpan'
      pure MkRecordLabel {..}

recordType :: Parser RecordType 
recordType = do 
  recordTypeLabels <- do
    between (lstring "{") (string "}") (sepBy1 (l recordLabel) $ lstring ",")
  pure MkRecordType {..}

type' :: Parser Type
type' = 
    choice [try tunit, trecord, tarray, tstring, tname]
  where
    trecord = TRecord <$> recordType 
    tarray = between (lstring "[") (string "]") (TArray <$> type')
    tstring = string "String" $> TString
    tname = TName <$> name 
    tunit = string "{}" $> TUnit

param :: Parser Param
param = 
    choice [dyn, typed]
  where
    dyn = do
      lstring_ "?"
      (paramNameSpan, paramName) <- lsrc name
      let paramTypeSpan = Nothing
          paramType     = TDynamic
      pure MkParam {..}
    typed = do
      (paramNameSpan, paramName) <- lsrc name
      lstring_ ":"
      (paramTypeSpan', paramType) <- lsrc type'
      let paramTypeSpan = Just paramTypeSpan'
      pure MkParam {..}

params :: Parser [Param]
params = parens . sepBy param $ lstring ","

recordField :: Parser RecordField
recordField = do
  (recordFieldNameSpan, recordFieldName) <- lsrc name
  lstring_ "="
  (recordFieldValueSpan, recordFieldValue) <- src expr
  pure MkRecordField {..}

recordValue :: Parser RecordValue
recordValue = do
  recordValueFields <- between (lstring "{") (string "}") (sepBy (l recordField) $ lstring ",")
  pure MkRecordValue {..}

expr :: Parser Expr
expr = 
    choice [try eunit, try eselect, erecord, try einvoke, estring, ename]
  where
    erecord = ERecord <$> recordValue
    eselect = ESelect <$> do
      recordSelectRecord <- choice [erecord, ename, try einvoke {-, TODO: Allow chained record selection -}]
      string_ "."
      (recordSelectLabelSpan, recordSelectLabel) <- src name
      pure MkRecordSelect {..} 
    einvoke =
      EInvoke
        <$> choice [ename {-, TODO: Allow chained function invocation-}]
        <*> (parens . sepBy expr $ lstring ",")  
    estring = do
      string_ "\""
      xs <- manyTill anySingle $ string "\""
      pure . EString $ T.pack xs 
    ename = EName <$> name 
    eunit = string "{}" $> EUnit 

localDef :: Parser LocalDef
localDef = do
  lstring_ "local"
  (localDefSpan, localDefName) <- lsrc name
  (localDefTypeSpan, localDefType) <- do 
    option (Nothing, Nothing) 
      $ bimap Just Just <$> (lstring ":" *> lsrc type')
  lstring_ "="
  (localDefValSpan, localDefVal) <- src expr
  pure MkLocalDef {..}

stmt :: Parser Stmt
stmt = 
    choice [local, return', stmt']
  where
    local = SLocalDef <$> localDef 
    return' = fmap SReturn $ lstring "return" *> expr
    stmt' = Stmt <$> expr 

stmts :: Parser [Stmt]
stmts = 
    manyTill (lh stmt <* lineTerminator) stmtsTerminator  
  where
    lineTerminator = void (l newline) <|> void (lstring ";")
    stmtsTerminator = lookAhead $ string "end"

funDef :: Parser FunDef
funDef = do
    lstring_ "function"
    (funDefNameSpan, funDefName) <- lsrc name
    funDefParams <- l params
    (funDefRetTypeSpan, funDefRetType) <- retty
    (funDefBodySpan, funDefBody) <- lsrc stmts 
    lstring_ "end"
    pure MkFunDef {..}
  where 
    retty = option 
      (Nothing, TUnit) 
      (lstring ":" >> first Just <$> lsrc type') 

-- | Attempts to parse an 'Ast' from the given source file. 
parseAst :: Source -> Either (ParseErrorBundle T.Text Void) Ast
parseAst src'@MkSource {..} =
    MkAst src' <$> parse (many def <* eof) sourcePath sourceContent
  where
    def = l $ choice [DLocalDef <$> localDef, DFunDef <$> funDef]