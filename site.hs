{-# LANGUAGE OverloadedStrings #-}

-- import Hakyll.Typescript.TS (compressJtsCompiler)

import Control.Monad (filterM)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hakyll
--import getUnderlying (it is not exported from Hakyll.Web.Page)
import Hakyll.Core.Compiler (getUnderlying)
import Hakyll.Web.Sass (sassCompiler)

--------------------------------------------------------------------------------

{-
 - Hakyll file for building willmatthews.xyz
 - Author: WillMatthews
 - Date: 2024-02-21
-}

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

  -- Does not work! Does not do anything! Why??
  match postPattern $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "page"
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

  match "images/*" $ do
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

-- | Context for posts
-- Posts have a created date, and a modified date
-- These are named in the front matter of the markdown file as "created" and "modified"
-- Hakyll has a dateField function that can be used to format the date but it
-- requires a 'published' field. This is a workaround to use the 'created' field
-- as the published date. using dateField "created" "%B %e, %Y" does not work, and
-- I don't know why.
postCtx :: Context String
postCtx =
  dateField "created" "%B %e, %Y"
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
      feedDescription = "This feed provides William Matthews' website.",
      feedAuthorName = "William Matthews",
      feedAuthorEmail = "test@example.com",
      feedRoot = "http://willmatthews.xyz"
    }
