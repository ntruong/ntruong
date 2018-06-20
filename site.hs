--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Control.Monad       (liftM)
import           Data.Binary
import           Data.List
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing)
import qualified Data.Set as S
import           Data.Typeable
import           Hakyll
import           Text.Pandoc.Options
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "css/**" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "static/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "index.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  create ["notes/index.html"] $ do
    route idRoute
    compile $ do
      notes <- loadSorted $ indexPattern "notes/*"
      let ctx = constField "title" "Notes" <>
                (indexCtx notes)

      makeItem ""
        >>= loadAndApplyTemplate "templates/book.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  notebook "notes/algebra" "Algebraic Structures"

  match "templates/*" $ compile templateBodyCompiler

notebook :: String -> String -> Rules ()
notebook id title = do
  match (indexPattern id) $ do
    route $ setExtension "html"
    compile $ do
      notes <- loadSorted $ pagesPattern id
      let ctx = constField "title" title <>
                (indexCtx notes)

      makeItem ""
        >>= loadAndApplyTemplate "templates/book.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tags <- buildTags (pagesPattern id) $ fromCapture "tags/*.html"

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      notes <- loadAll pattern
      let ctx = constField "title" ("[" ++ tag ++ "]") <>
                indexCtx (return notes)

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match (pagesPattern id) $ do
    route $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/article.html" (noteCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" (noteCtx tags)
      >>= relativizeUrls

indexCtx :: Compiler [Item String] -> Context String
indexCtx pages = listField "pages" defaultContext pages <>
                 defaultContext

noteCtx :: Tags -> Context String
noteCtx tags = tagsField "tags" tags <> defaultContext

indexPattern :: String -> Pattern
indexPattern id = fromGlob $ id ++ "/index.md"

pagesPattern :: String -> Pattern
pagesPattern id = (fromGlob $ id ++ "/*") .&&. complement (indexPattern id)

loadSorted :: (Binary a, Typeable a, MonadMetadata m) =>
              Pattern -> Compiler (m [Item a])
loadSorted pattern = fmap (sortMd "ordering") $ loadAll pattern

-- | Sorts items by a specified metadata field (with ordering)
sortMd :: MonadMetadata m => String -> [Item a] -> m [Item a]
sortMd field =
  sortByM $ \x -> getMetadataField (itemIdentifier x) field
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

-- | Compile template with LaTeX
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
