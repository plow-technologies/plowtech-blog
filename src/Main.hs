{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except  (throwError)
import           Data.Aeson
import           Data.Binary
import qualified Data.ByteString.Lazy  as BL
import           Data.Function
import           Data.List
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.Text             hiding (reverse)
import           Data.Text.Read
import           Data.Time.Calendar
import           Data.Typeable
import qualified Data.Yaml             as Y
import           GHC.Generics
import           Hakyll
import           System.FilePath.Posix
import           Text.Pandoc

postReaderOptions :: ReaderOptions
postReaderOptions = def {
                      readerExtensions=githubMarkdownExtensions `Set.difference` [ Ext_hard_line_breaks ]
                    }

postWriterOptions :: WriterOptions
postWriterOptions = def {
                      writerHighlight = False
                    , writerStandalone = False
                    }

hakyllConfiguration :: Configuration
hakyllConfiguration = def {
                        deployCommand = "./publish.sh"
                      }
main :: IO ()
main = do
  hakyllWith hakyllConfiguration $ do
    match "templates/*" $ compile templateCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "css/layouts/*" $ do
      route idRoute
      compile compressCssCompiler
    match "highlighting/*.js" $ do
      route idRoute
      compile copyFileCompiler
    match "highlighting/styles/*" $ do
      route idRoute
      compile copyFileCompiler
    match "posts/*.yaml" $ do
      route $ setExtension "html"
      compile postConfigCompiler
    match "posts/*.md" $ do
      route $ setExtension "html"
      compile $ postCompiler
                >>= loadAndApplyTemplate "templates/post.html" postContext
    match "posts/*.gif" $ do
      route idRoute
      compile $ copyFileCompiler
    create ["index.html"] $ do
      route idRoute
      compile compileMain


data Post = Post {
  postConfig  :: PostConfig
, postContent :: String
} deriving (Show, Eq, Ord, Typeable, Generic)


data PostConfig = PostConfig {
  postConfTitle       :: String
, postConfAuthor      :: String
, postConfDescription :: String
, postConfTags        :: [String]
, postConfCreated     :: PostDay
} deriving (Read, Show, Eq, Ord, Typeable, Generic)

instance Binary PostConfig where
instance Writable PostConfig where
  write _ _ = return ()

instance FromJSON PostConfig where
  parseJSON (Object v) = PostConfig <$>
                          v .: "title" <*>
                          v .: "author" <*>
                          v .: "description" <*>
                          v .: "tags" <*>
                          v .: "created"
  parseJSON _ = mzero

instance Binary Post where
instance Writable Post where
  write fp item = writeFile fp $ postContent $ itemBody item

newtype PostDay = PostDay { unPostDay :: Day } deriving (Read, Eq, Show, Ord, Typeable, Generic)

instance Binary PostDay where
  put (PostDay day) = put $ toGregorian day
  get = (\(y,m,d) -> PostDay $ fromGregorian y m d) <$> get

instance FromJSON PostDay where
  parseJSON (String txt) = case eDay of
                            (Left str) -> fail str
                            (Right day) -> return $ PostDay day
                           where eDay = case splitOn "-" txt of
                                          (year:month:day:_) -> fromGregorian <$> parsedEYear <*> parsedEMonth <*> parsedEDay
                                                                  where parsedEYear = fst <$> decimal year
                                                                        parsedEMonth = fst <$> decimal month
                                                                        parsedEDay = fst <$> decimal day
                                          _ -> Left "Expected day in year-month-day form"
  parseJSON _ = mzero


fromPostConfig :: PostConfig -> String -> Post
fromPostConfig = Post



postConfigCompiler :: Compiler (Item PostConfig)
postConfigCompiler = do
  fConts <- getResourceLBS
  fp <- getResourceFilePath
  case Y.decodeEither . BL.toStrict $ itemBody fConts of
    (Right postConf) -> return $ itemSetBody postConf fConts
    (Left err) -> throwError ["Error when compiling: " ++ fp ++ " " ++ err]

postCompiler :: Compiler (Item Post)
postCompiler = do
  fp <- getResourceFilePath
  postConf <- load $ fromFilePath $ Prelude.drop 2 $ replaceExtension fp "yaml"
  compilerRenderedPost <- renderPandocWith postReaderOptions postWriterOptions <$> getResourceString
  renderedPost <- compilerRenderedPost
  makeItem $ fromPostConfig (itemBody postConf) (itemBody renderedPost)


postContext :: Context Post
postContext = title <> author <> description <> body <> url <> path <> date
  where title = field "title" (return . postConfTitle . postConfig . itemBody)
        author = field "author" (return . postConfAuthor . postConfig . itemBody)
        description = field "description" (return . postConfDescription . postConfig . itemBody)
        date = field "date" (return . show . unPostDay . postConfCreated . postConfig . itemBody)
        body = field "body" (return . postContent . itemBody)
        url = urlField "url"
        path = pathField "path"


postConfigContext :: Context PostConfig
postConfigContext = title <> author <> description <> url <> path <> date
  where title = field "title" (return . postConfTitle . itemBody)
        author = field "author" (return . postConfAuthor . itemBody)
        description = field "description" (return . postConfDescription . itemBody)
        date = field "date" (return . show . unPostDay . postConfCreated . itemBody)
        url = urlField "url"
        path = pathField "path"



compileMain :: Compiler (Item String)
compileMain = compileDescriptions (Data.List.take 5)

compileArchive :: Compiler (Item String)
compileArchive = compileDescriptions id

compileDescriptions :: (forall a.[a] -> [a]) -- Used to make the main page look nice while the archive can just use id
                        -> Compiler (Item String)
compileDescriptions dropSome = do
  postConfigs <- loadAll "posts/*.yaml" :: Compiler [Item PostConfig]
  let sortedConfigs = reverse $ sortBy (compare `on` (postConfCreated . itemBody)) postConfigs
      topFive = dropSome sortedConfigs :: [Item PostConfig]
  descTemplate <- loadBody "templates/description.html"
  descriptions <- applyTemplateList descTemplate postConfigContext topFive
  loadAndApplyTemplate "templates/index.html" defaultContext =<< makeItem descriptions
