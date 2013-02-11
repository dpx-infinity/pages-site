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
    
    -- Favicon
    ["favicon.ico"] ++> copy
    
    -- Images
    ["images/**"] ++> copy
    
    -- Static files
    ["static/**"] ++> copy
        
    -- Javascript files
    ["js/**"] ++> copy
        
    -- CSS
    ["styles/*"] ++> do
        route idRoute
        compile compressCssCompiler
    
    -- Templates
    ["templates/*"] ++> compile templateCompiler
    
    -- Default context
    let defaultContext = mconcat [ bodyField "body"
                                 , metadataField
                                 , urlField "url"
                                 , pathField "path"
                                 ]
    
    -- Posts
    ["posts/**"] ++> do
        route $ setExtension "html"
        compile $
            pandocCompilerWith readerOptions writerOptions
            >>= saveSnapshot "postBody"
            >>= loadAndApplyTemplate "templates/post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls        
        
    -- Toplevel
    ["*.md", "*.html", "*.lhs"] ++> do
        route $ setExtension "html"
        compile $ do
            pandocCompilerWith readerOptions writerOptions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            
    -- Prepare tags metainformation
    tags <- buildTags "posts/**" (fromCapture "tags/*")

    -- Bind tags metainfo-dependent functions
    let
        smallPostTagListField name = field name $ \item -> do
            getTags (itemIdentifier item) >>= renderSmallPostTagList

        renderSmallPostTagList =
            renderPostTags (renderClassedUrl "tag-small") (intercalate ", ") tags
        
        fullPostsList n = do
            postTemplate <- loadBody "templates/post.html"
            posts <- (return . take n <=< newestFirst) =<< loadAllSnapshots "posts/**" "postBody"
            applyTemplateList postTemplate defaultContext posts

        allDatedPostTitlesList = do
            postItemTemplate <- loadBody "templates/dated-post-item.html"
            posts <- newestFirst =<< loadAllSnapshots "posts/**" "postBody"
            applyTemplateList postItemTemplate (smallPostTagListField "tags" <> defaultContext) posts

    -- The body will be executed for each tag with matching pattern; corresponding file
    -- will be created
    -- Here we have to create pages for each tag
    tagsRules tags $ \tag pattern -> do
        route $ setExtension "html"
        compile $ do
            makeItem ("" :: String)

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
            postsPagePosts <- allDatedPostTitlesList
            let postsContext = constField "posts" postsPagePosts <> defaultContext

            -- Render posts page
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    where
        --- Simple template function, useful for generating tags
        renderClassedUrl :: String -> String -> String -> String
        renderClassedUrl clazz tag url =
            printf "<a class=\"%s\" href=\"%s\">%s</a>" clazz url tag
    