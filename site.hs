--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
-- import Hakyll.Typescript.TS (compressJtsCompiler)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
--------------------------------------------------------------------------------

{-
 - Hakyll file for building willmatthews.xyz
 - Author: WillMatthews
 - Date: 2024-02-21
-}


main :: IO ()
main = hakyll $ do
  -- meta <- getMetadata "posts/index.md"

  -- match (fromList ["posts/index.md", "posts/me.md"]) $ do
  --     -- move the file to the root of the site
  --     route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
  --     compile $ pandocCompiler
  --         >>= loadAndApplyTemplate "templates/post.html"    postCtx
  --         >>= saveSnapshot "content"
  --         >>= loadAndApplyTemplate "templates/default.html" postCtx
  --         >>= relativizeUrls

  let postPattern = fromGlob "posts/*.md" .&&. complement "posts/README.md"

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

  -- Does not work! Does not do anything!
  -- match postPattern $ do
  --   route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "page"
  --   compile copyFileCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <>
            constField "description" "This is the post description"

      posts <- fmap (take 10) . recentFirst =<<
           loadAllSnapshots "posts/*" "content"
      renderAtom myFeedConfiguration feedCtx posts

  create ["index.xml"] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx <>
                    constField "description" "This is the post description"

          posts <- fmap (take 10) . recentFirst =<<
              loadAllSnapshots "posts/*" "content"
          renderRss myFeedConfiguration feedCtx posts

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts) <>
              constField "title" "Archives" <>
              defaultContext

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
    compile $ sassCompiler
      >>= return . fmap compressCss

  -- match "typescript/**" $ do
  --   route $ setExtension "js"
  --   compile compressJtsCompiler

--------------------------------------------------------------------------------


-- "created" and "modified" are metadata fields in the markdown file
-- They have the format "YYYY-MM-DD"
--
-- Example:
-- created: 2018-01-19T16:50:20Z
-- modified: 2024-02-22
--
postCtx :: Context String
postCtx =
  -- dateField "created" "%Y-%m-%d"
  --   <> dateField "modified" "%Y-%m-%d"
     defaultContext


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "William Matthews - EE, ML, SW",
      feedDescription = "This feed provides William Matthews' website.",
      feedAuthorName = "William Matthews",
      feedAuthorEmail = "test@example.com",
      feedRoot = "http://willmatthews.xyz"
    }
