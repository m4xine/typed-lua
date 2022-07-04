module Source 
  ( Source(..)
  , readSource
  , SrcSpan(..)
  , srcSpan
  ) where 

import           Control.Monad.IO.Class       (liftIO, MonadIO)
import           Control.Exception            (assert)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Text.Megaparsec              (SourcePos(..), unPos)

-- | Representation of a source file.
data Source = MkSource
  { sourcePath    :: FilePath
  , sourceContent :: T.Text 
  } deriving Show

-- | Attempts to read a source file from the given 'FilePath'. 
readSource :: MonadIO m => FilePath -> m Source
readSource path =
  liftIO $ MkSource path <$> T.readFile path

-- | Spanned region within a source file.
data SrcSpan = MkSrcSpan
  { spanPath  :: FilePath
  , spanBegin :: !(Int, Int)
  , spanEnd   :: !(Int, Int)
  } deriving (Show, Eq)

-- | Constructs a 'SrcSpan' from two source positions within a source file.
-- The first provided 'SourcePos' must be equal to or before the second
-- provided 'SourcePos'.  
srcSpan 
  :: SourcePos 
  -> SourcePos 
  -> SrcSpan
srcSpan (SourcePos p bl bc) (SourcePos p' el ec) = 
    assert cond $ MkSrcSpan p (unPos bl, unPos bc) (unPos el, unPos ec)
  where 
    cond = p == p' && (bl < el || (bl == el && bc <= ec))