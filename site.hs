--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Control.Monad       (liftM)
import           Data.Binary
import           Data.List
import           Data.Monoid         ((<>))
import qualified Data.Set as S
import           Hakyll
import           Text.Pandoc.Options
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

  match "index.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= relativizeUrls

  match "CNAME" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/**" $ do
    route   idRoute
    compile compressCssCompiler

  match "static/**" $ do
    route   idRoute
    compile copyFileCompiler

  tags <- buildTags (fromGlob "notes/**") (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      let ctx = contexts [ indexCtx (loadAll pattern)
                         , constField "title" (":" ++ tag) ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "notes/**" $ do
    route $ setExtension "html"
    compile $ do
      let ctx = contexts [ cssCtx ["css/article.css"]
                         , noteCtx tags
                         ]

      pandocMathCompiler
        >>= loadAndApplyTemplate "templates/article.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  create ["notes/index.html"] $ do
    route idRoute
    compile $ do
      let notesPattern = (fromGlob "notes/**") .&&. complement "notes/index.html"
          ctx = contexts [ cssCtx ["css/article.css"]
                         , (indexCtx . loadAll) notesPattern
                         , constField "title" "Notes"
                         ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/book.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

indexCtx :: Compiler [Item String] -> Context String
indexCtx = listField "pages" defaultContext

noteCtx :: Tags -> Context String
noteCtx = tagsField "tags"

cssCtx :: [Identifier] -> Context String
cssCtx styles = listField "css" defaultContext (loadAll $ fromList styles)

contexts :: [Context String] -> Context String
contexts ctxs = mconcat ctxs <> defaultContext

--------------------------------------------------------------------------------

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
