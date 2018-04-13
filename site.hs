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

  match "static/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "index.html" $ do
    route idRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  create ["notes/index.html"] $ do
    route idRoute
    compile $ do
      notes <- loadToCSorted $ indexPattern "notes/*"
      let notebookCtx = listField "ToC" defaultContext notes `mappend`
                        constField "return" "Home" `mappend`
                        constField "title" "Notes" `mappend`
                        defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/notebook.html" notebookCtx
        >>= loadAndApplyTemplate "templates/default.html" notebookCtx
        >>= relativizeUrls

  notebook "notes/algebra"

  match "templates/*" $ compile templateBodyCompiler

notebook :: String -> Rules ()
notebook id = do
  match (indexPattern id) $ do
    route $ setExtension "html"
    compile $ do
      notes <- loadToCSorted $ pagesPattern id
      let notebookCtx = listField "ToC" defaultContext notes `mappend`
                        constField "return" "Notes" `mappend`
                        defaultContext

      pandocMathCompiler
        >>= loadAndApplyTemplate "templates/notebook.html" notebookCtx
        >>= loadAndApplyTemplate "templates/default.html" notebookCtx
        >>= relativizeUrls

  match (pagesPattern id) $ do
    route $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/note.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

indexPattern :: String -> Pattern
indexPattern id = fromGlob $ id ++ "/index.md"

pagesPattern :: String -> Pattern
pagesPattern id = (fromGlob $ id ++ "/*") .&&. complement (indexPattern id)

loadToC :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadToC pat = loadAll $ pat

loadToCSorted :: (Binary a, Typeable a, MonadMetadata m) =>
                 Pattern -> Compiler (m [Item a])
loadToCSorted pat = fmap (sortMd "ordering") $ loadToC pat

sortMd :: MonadMetadata m => String -> [Item a] -> m [Item a]
sortMd field =
  sortByM $ \x -> getMetadataField (itemIdentifier x) field
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

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
