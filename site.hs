{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)

-- import Hakyll.Typescript.TS (compressJtsCompiler)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  let postPattern = fromGlob "posts/*.md" .&&. complement "posts/README.md"
  let numRSSItems = 25

  -- match posts that are not index.md
  -- if the file is named "README.md", ignore it, otherwise process it
  match postPattern $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  -- Does not work! Does not do anything! Why???
  -- Goal: Copy the post.md to a new file called post.page
  match "posts/images/**" $ do
    route $ gsubRoute "posts/" (const "")
    compile copyFileCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      posts <-
        recentFirst
          =<< filterOutDrafts
          =<< loadAllSnapshots "posts/*" "content"

      let feedCtx =
            postCtx
              <> constField "description" "This is the post description"
      renderAtom myFeedConfiguration feedCtx (take numRSSItems posts)

  create ["index.xml"] $ do
    route idRoute

    compile $ do
      posts <-
        recentFirst
          =<< filterOutDrafts
          =<< loadAllSnapshots "posts/*" "content"

      let feedCtx =
            postCtx
              <> constField "description" "This is the post description"
      renderRss myFeedConfiguration feedCtx (take numRSSItems posts)

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <-
        recentFirst
          =<< filterOutDrafts
          =<< loadAll "posts/*"

      let archiveCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Archives"
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["404.html"] $ do
    route idRoute
    compile $ do
      let notFoundCtx = constField "title" "404" <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/404.html" notFoundCtx
        >>= loadAndApplyTemplate "templates/default.html" notFoundCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  -- TODO: scaling of images wih https://hackage.haskell.org/package/hakyll-images-1.2.1/docs/Hakyll-Images-Resize.html
  -- TODO: Dithering of images with Weighted Voronoi Stippling (https://www.cs.ubc.ca/labs/imager/tr/2002/secord2002b/secord.2002b.pdf)
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "scss/main.scss" $ do
    route $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
    compile $
      sassCompiler
        >>= return . fmap compressCss

-- match "typescript/**" $ do
--   route $ setExtension "js"
--   compile compressJtsCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  -- format as yyyy-mm-dd
  dateField "date" "%Y-%m-%d"
    <> defaultContext

filterOutDrafts :: MonadMetadata m => [Item a] -> m [Item a]
filterOutDrafts = filterM isPublished
  where
    isPublished item = do
      metadata <- getMetadata (itemIdentifier item)
      return $ case lookupString "draft" metadata of
        Just "true" -> False
        _ -> True

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "William Matthews - EE, ML, SW",
      feedDescription = "This if the feed for willmatthews.xyz, the personal website of William Matthews. Contains posts about electrical engineering, machine learning, and software.",
      feedAuthorName = "William Matthews",
      feedAuthorEmail = "test@example.com",
      feedRoot = "http://willmatthews.xyz"
    }
