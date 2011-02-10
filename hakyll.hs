import Control.Monad (liftM)
import Control.Monad.Reader (asks)
import Control.Arrow ((>>>))
import Data.List (sort)
import Data.Either (Either(..))
import System.FilePath (joinPath)

import Text.Hakyll (hakyll)
import Text.Hakyll.HakyllMonad (HakyllConfiguration (..))
import Text.Hakyll.File (directory, getRecursiveContents)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.CreateContext (createPage, createListing)
import Text.Hakyll.ContextManipulations (renderValue, renderDate)

main = hakyll "http://mazzo.li/b/" $ do
  directory static "css"
  directory static "images"
  directory static "js"
  
  articlesPaths <- liftM (reverse . sort) $ getRecursiveContents "articles"
  absurl <- asks absoluteUrl
  let stripQuotes ""      = ""
      stripQuotes (c : s) | c == '\''  = "\\'" ++ stripQuotes s
                          | otherwise = c : stripQuotes s
      articlesPages = map (
        (>>> renderDate "prettydate" "%b %e, %Y" "Date unknown") .
        (>>> renderValue "path" "identifier" stripQuotes) .
        (>>> renderValue "url" "absurl" (\u -> joinPath [absurl, u])) .
        createPage) articlesPaths

  mapM_ (renderChain ["templates/article.html", "templates/default.html"])
    articlesPages
       
  let index = createListing "index.html"
              ["templates/articlelink.html"]
              articlesPages
              [("title", Left "Home")]
              
  renderChain ["templates/index.html", "templates/default.html"] index