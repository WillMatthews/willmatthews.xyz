--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Hakyll.Web.Sass (sassCompiler)
-- import Hakyll.Typescript.TS (compressJtsCompiler)

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

  -- match posts that are not index.md
  -- if the file is named "README.md", ignore it, otherwise process it
  match (fromGlob "posts/*.md" .&&. complement "posts/README.md") $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

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
-- The dateField function formats the date to a human-
postCtx :: Context String
postCtx =
  dateField "created" "%B %e, %Y" <>
    dateField "modified" "%B %e, %Y" <>
    modificationTimeField "modified" "%B %e, %Y" <>
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

-- type Snapshot = String
-- saveSnapshot :: (Typeable a, Binary a)
--            => Snapshot -> Item a -> Compiler (Item a)
