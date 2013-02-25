{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import Data.Function
import Data.List
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.IO.Unsafe
import Text.Printf (printf)

import Text.Pandoc.Options (WriterOptions(..), ReaderOptions(..))
import Hakyll
import Hakyll.Web.Pandoc
import Hakyll.Core.Util.String

p :: (Show a) => a -> a
p x = unsafePerformIO $ print x >> return x

ps ++> f = mapM_ (\p -> match p f) ps

-- Custom parser state for page reader.
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

-- Custom writer options for page generator.
writerOptions :: WriterOptions 
writerOptions = defaultHakyllWriterOptions {
    writerHtml5 = True
}

-- A number of recent posts to show.
recentPostsNumber :: Int
recentPostsNumber = 8

-- Sorts post pages by their path and possibly ordering field in ascending order.
oldestFirst :: (Functor m, MonadMetadata m) => [Item a] -> m [Item a]
oldestFirst items =
    map fst <$> sortBy compareDates <$> zip items <$> mapM (getMetadata . itemIdentifier) items
    where
        pagePath = toFilePath . itemIdentifier
        pageOrd = M.findWithDefault "0" "ord"

        compareDates (i1, meta1) (i2, meta2) =
            let (path1, ord1) = (pagePath i1, pageOrd meta1)
                (path2, ord2) = (pagePath i2, pageOrd meta2)
                f1@[_, y1, m1, d1, name1, o1] = splitAll "/" path1 ++ [ord1]
                f2@[_, y2, m2, d2, name2, o2] = splitAll "/" path2 ++ [ord2]
            in compare f1 f2

-- Sorts post pages by their path and possibly ordering field in descending order.
newestFirst :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
newestFirst = fmap reverse . oldestFirst

-- | This function is very similar to 'renderTags'; it renders a list of tags too,
-- but not all of them, only specified ones
-- Useful for rendering tags for single post
renderPostTags :: (String -> String -> String)
               -- ^ Generate HTML representation: tag, url -> html
               -> ([String] -> String)
               -- ^ Concatenate several tags
               -> Tags
               -- ^ Tags metadata
               -> [String]
               -- ^ Tags for the post               
               -> Compiler String
               -- ^ Resulting compiler
renderPostTags generate concatenate tags postTags = do
    postTagsInfo <- fmap (filter ((`elem` postTags) . fst)) . forM (tagsMap tags) $ \(tag, ids) -> do
        tagPageRoute <- getRoute $ tagsMakeId tags tag  -- future url
        let tagUrl = toUrl $ maybe "/" id tagPageRoute
        return (tag, tagUrl)
    return $ concatenate $ map (uncurry generate) postTagsInfo
    

main :: IO ()
main = hakyll $ do
    -- Identity rule, copy as is
    let copy = route idRoute >> compile copyFileCompiler
    
    -- Static files
    ["favicon.ico", "images/**", "static/**", "js/**"] ++> do
        route idRoute
        compile copyFileCompiler
    
    -- CSS
    ["styles/*"] ++> do
        route idRoute
        compile compressCssCompiler
    
    -- Templates
    ["templates/*"] ++> compile templateCompiler
    
    -- Default context
    let defaultContext = mconcat [ bodyField "body"
                                 , constField "globalTitle" "Boring programmer's notes"
                                 , metadataField
                                 , urlField "url"
                                 , pathField "path"
                                 ]

    -- Toplevel
    ["*.md", "*.html", "*.lhs"] ++> do
        route $ setExtension "html"
        compile $ do
            pandocCompilerWith readerOptions writerOptions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            
    -- Prepare tags metainformation
    tags <- buildTags "posts/**" (fromCapture "tags/*.html")

    -- Bind tags metainfo-dependent functions
    let
        smallPostTagListField name = field name $ \item -> do
            getTags (itemIdentifier item) >>= renderSmallPostTagList

        renderSmallPostTagList =
            renderPostTags (renderClassedUrl "tag-small") (intercalate ", ") tags
        
        fullPostsList n = do
            postTemplate <- loadBody "templates/post.html"
            posts <- (return . take n <=< newestFirst) =<< loadAllSnapshots "posts/**" "postBody"
            applyTemplateList postTemplate (smallPostTagListField "tags" <> defaultContext) posts

        postTitlesWithDateWithTagsList postsPattern = do
            postItemTemplate <- loadBody "templates/dated-post-item.html"
            posts <- newestFirst =<< loadAllSnapshots postsPattern "postBody"
            applyTemplateList postItemTemplate (smallPostTagListField "tags" <> defaultContext) posts

        allPostTitlesWithDateWithTagsList = postTitlesWithDateWithTagsList "posts/**"

        postTitlesWithDateList postsPattern = do
            postItemTemplate <- loadBody "templates/dated-post-item.html"  -- TODO: use another template
            posts <- newestFirst =<< loadAllSnapshots postsPattern "postBody"
            applyTemplateList postItemTemplate (constField "tags" "" <> defaultContext) posts

    -- The body will be executed for each tag with matching pattern; corresponding file
    -- will be created
    -- Here we have to create pages for each tag
    tagsRules tags $ \tag pattern -> do
        route $ setExtension "html"
        compile $ do
            tagPagePosts <- postTitlesWithDateWithTagsList pattern
            let tagContext = constField "posts" tagPagePosts
                             <> constField "title" ("Posts tagged " ++ tag)
                             <> defaultContext
            
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" tagContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Tags list page
    create ["tags.html"] $ do
        route idRoute
        compile $ do
            tagsList <- renderTags fullTagsListRenderer fullTagsListJoiner tags
            -- I know, it is weird to save tags list under "posts" variable,
            -- but I'm too lazy to create new template only for tags
            let tagsContext = constField "posts" tagsList  
                              <> constField "title" "All tags"
                              <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" tagsContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Posts
    ["posts/**"] ++> do
        route $ setExtension "html"
        compile $ do
            pandocCompilerWith readerOptions writerOptions
            >>= saveSnapshot "postBody"
            >>= loadAndApplyTemplate "templates/post.html" (smallPostTagListField "tags" <> defaultContext)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Generate index file
    create ["index.html"] $ do
        route idRoute
        compile $ do
            -- Load a number of rendered posts and store them in the context
            indexPagePosts <- fullPostsList recentPostsNumber
            let indexContext = constField "posts" indexPagePosts <> defaultContext

            -- Render main page
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls    

    -- Generate all posts list file
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            -- Load a list of posts and store it in the context
            postsPagePosts <- allPostTitlesWithDateWithTagsList
            let postsContext = constField "posts" postsPagePosts
                               <> constField "title" "All posts"
                               <> defaultContext

            -- Render posts page
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Devourer manual
    ["devourer/*"] ++> do
        route $ setExtension "html"
        compile $ do
            pandocCompilerWith readerOptions writerOptions
            >>= loadAndApplyTemplate "templates/devourer.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" (constField "globalTitle" "Devourer library" <>
                                                               defaultContext)
            >>= relativizeUrls

    where
        --- Simple template function, useful for generating tags
        renderClassedUrl :: String -> String -> String -> String
        renderClassedUrl clazz tag url =
            printf "<a class=\"%s\" href=\"%s\">%s</a>" clazz url tag
        
        --- Render functions for tags list
        fullTagsListRenderer tag url count _ _ =
            printf "<div class=\"link-small-div\">%s</div>" $
                renderClassedUrl "tag-normal" tag url ++ printf " (%d)" count
        fullTagsListJoiner = intercalate "\n"
