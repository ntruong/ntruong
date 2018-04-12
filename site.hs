--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
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

  match "templates/*" $ compile templateBodyCompiler

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
