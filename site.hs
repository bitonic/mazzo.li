{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
import           Control.Monad
import           Data.Maybe (fromMaybe, isJust)
import           Agda.Interaction.Options (CommandLineOptions(..), defaultOptions)
import           Hakyll
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Web.Agda
import           Text.Pandoc.Options
import           KaTeXify
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import qualified Data.Text as T

main :: IO ()
main = hakyll $ do -- Assets
  match ("assets/images/*" .||. "assets/css/*" .||. "assets/js/*" .||. "assets/other/*" .||. "assets/webpack/*") $ do
    route idRoute
    compile copyFileCompiler

  -- Templates
  match "templates/*" $ do
    compile templateCompiler

  -- Posts
  match ("posts/*.md" .||. "posts/*.lagda" .||. "posts/*.lhs") $ do
    route $ setExtension "html"
    compile $
      pandocMathAndAgdaCompiler >>=
      loadAndApplyTemplate "templates/post.html" postCtx >>=
      saveSnapshot "content" >>=
      loadAndApplyTemplate "templates/default.html" postCtx >>=
      relativizeUrls

  -- Archive
  create ["archive.html"] $ do
    route idRoute
    let archiveCtx =
          field "posts" (\_ -> postList recentFirst) <> constField "title" "index" <> defaultContext
    compile $
      makeItem "" >>=
      loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
      loadAndApplyTemplate "templates/default.html" archiveCtx >>=
      relativizeUrls

  -- Splash page
  match "index.html" $ do
    route idRoute
    compile $ getResourceBody >>= relativizeUrls

  -- RSS and Atom
  create ["atom.xml"] $ renderFeed renderAtom
  create ["rss.xml"] $ renderFeed renderRss

  -- CV
  match "cv/*" $ do
    route idRoute
    compile copyFileCompiler

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%Y-%m-%d"
  , listFieldWith "tags" (field "tag" (return . itemBody)) $ \item -> do
      let identifier = itemIdentifier item
      meta <- getMetadata identifier
      let tags = fromMaybe ["post"] $ lookupStringList "tags" meta
      return $ map (Item identifier) tags
  , field "renderedTitle" $ \item -> do
      metadata <- getMetadata (itemIdentifier item)
      let str = fromMaybe "untitled" (lookupString "title" metadata)
      compilerUnsafeIO $ Pandoc.runIOorExplode $ do
        strWithParagraph <- Pandoc.readMarkdown Pandoc.def (T.pack str) >>= Pandoc.writeHtml5String Pandoc.def
        -- remove <p> and </p>
        return (T.unpack (T.reverse (T.drop (T.length "</p>") (T.reverse (T.drop (T.length "<p>") strWithParagraph)))))
  , defaultContext
  ]

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts <- filterM isPublished =<< sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl postCtx posts
  return list

isPublished :: (MonadMetadata m, MonadFail m) => Item a -> m Bool
isPublished (itemIdentifier -> ident) = do
  publishedM <- getMetadataField ident "published"
  case publishedM of
    Nothing -> return True
    Just "false" -> return False
    Just s -> fail ("invalid `published' metadata value: " ++ s)

renderFeed
  :: (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String))
  -> Rules ()
renderFeed f = do
  route idRoute
  let feedCtx = postCtx <> bodyField "description"
  compile $ do
    posts <- fmap (take 10) . recentFirst =<< filterM isPublished =<< loadAllSnapshots "posts/*" "content"
    f feedConf feedCtx posts

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
  { feedTitle       = "bitonic's blog."
  , feedDescription = "Often in error, never in doubt."
  , feedAuthorName  = "Francesco Mazzoli"
  , feedAuthorEmail = "f@mazzo.li"
  , feedRoot        = "http://mazzo.li"
  }

writerOpts :: Bool -> WriterOptions
writerOpts sidenotes = defaultHakyllWriterOptions
  { writerTableOfContents = True
  , writerExtensions =
      extensionsFromList mathExtensions <> writerExtensions defaultHakyllWriterOptions
  , writerHTMLMathMethod = KaTeX ""
  , writerReferenceLocation = if sidenotes then EndOfBlock else EndOfDocument
  }
  where
    mathExtensions =
      [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]

agdaOpts :: CommandLineOptions
agdaOpts = defaultOptions {optIncludePaths = [".", "../agdalib/src"]}

pandocMathAndAgdaCompiler :: Compiler (Item String)
pandocMathAndAgdaCompiler = do
  i <- getUnderlying
  katex <- getMetadataField i "katex"
  let katexTransform = if isJust katex then unsafeCompiler . kaTeXifyIO else return
  -- See <https://frasertweedale.github.io/blog-fp/posts/2020-12-10-hakyll-section-links.html>
  let sectionLinkTransform :: Monad m => Pandoc.Pandoc -> m Pandoc.Pandoc
      sectionLinkTransform = return . Pandoc.walk f where
        f (Pandoc.Header n attr@(idAttr, _, _) inlines) | n > 1 =
          let link = Pandoc.Link ("", ["section-link"], []) [Pandoc.Str "#"] ("#" <> idAttr, "")
          in Pandoc.Header n attr (inlines <> [Pandoc.Space, link])
        f x = x
  sidenotes <- maybe False (== "true") <$> getMetadataField i "sidenotes"
  pandocAgdaCompilerWithTransformM defaultHakyllReaderOptions (writerOpts sidenotes) agdaOpts (sectionLinkTransform >=> katexTransform)