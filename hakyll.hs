import Control.Monad (liftM)
import Control.Arrow ((>>>))
import Data.List (sort)
import Data.Either (Either(..))

import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory, getRecursiveContents)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.CreateContext (createPage, createListing)
import Text.Hakyll.ContextManipulations (renderDate)

main = hakyll "http://mazzo.li" $ do
  directory static "css"
  directory static "images"
  directory static "js"
  
  articlesPaths <- liftM (reverse . sort) $ getRecursiveContents "articles"
  let articlesPages = map ((>>> renderDate "prettydate" "%b %e, %Y" "Date unknown") .
                           createPage) articlesPaths

  let index = createListing "index.html"
              ["templates/articlelink.html"]
              articlesPages
              [("title", Left "Home")]
              
  renderChain ["templates/index.html", "templates/default.html"] index
  
  mapM_ (renderChain ["templates/article.html", "templates/default.html"])
    articlesPages