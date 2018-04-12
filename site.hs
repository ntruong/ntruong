--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Control.Monad       (liftM)
import           Data.Binary
import           Data.List
import           Data.Monoid         (mappend)
import           Data.Ord            (comparing)
import qualified Data.Set as S
import           Data.Typeable
import           Hakyll
import           Text.Pandoc.Options
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "notes/*" $ do
    route   $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/note.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "static/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "index.html" $ do
    route idRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  notebook "algebra"

  match "templates/*" $ compile templateBodyCompiler

noteIndexPattern :: String -> Pattern
noteIndexPattern id = fromGlob $ "notes/" ++ id ++ "/index.md"

notePattern :: String -> Pattern
notePattern id = (fromGlob $ "notes/" ++ id ++ "/*") .&&.
                 complement (noteIndexPattern id)

loadNotesSorted :: (Binary a, Typeable a, MonadMetadata m) =>
                   String -> Compiler (m [Item a])
loadNotesSorted id = fmap sortNotes $ loadNotes id

loadNotes :: (Binary a, Typeable a) => String -> Compiler [Item a]
loadNotes id = loadAll $ (notePattern id) .&&. hasVersion "toc"

loadNotebook :: (Binary a, Typeable a) => String -> Compiler (Item a)
loadNotebook id = fmap head . loadAll $ (noteIndexPattern id) .&&.
                  hasVersion "toc"

sortNotes :: MonadMetadata m => [Item a] -> m [Item a]
sortNotes =
  sortByM $ \x -> getMetadataField (itemIdentifier x) "ordering"
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

compileNoteTemplate :: String -> String -> Rules ()
compileNoteTemplate id template = compile $ do
  notes <- loadNotesSorted id
  let notebookCtx = listField "notes" defaultContext notes `mappend`
                    defaultContext

  pandocMathCompiler
    >>= loadAndApplyTemplate (fromFilePath template) notebookCtx
    >>= loadAndApplyTemplate "templates/default.html" notebookCtx
    >>= relativizeUrls


notebook :: String -> Rules ()
notebook id = do
  let compileTemplate = compileNoteTemplate id
  match (noteIndexPattern id) $ do
    route $ setExtension "html"
    compileTemplate "templates/notebook.html"

  match (notePattern id) $ do
    route $ setExtension "html"
    compileTemplate "templates/note.html"

  match (notePattern id) $ version "toc" $ do
    route $ setExtension "html"
    compile getResourceBody

pandocMathCompiler =
  let mathExtensions = [Ext_tex_math_dollars,
                        Ext_tex_math_double_backslash,
                        Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr S.insert defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions {
           writerExtensions = newExtensions,
           writerHTMLMathMethod = MathJax ""
           }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions
