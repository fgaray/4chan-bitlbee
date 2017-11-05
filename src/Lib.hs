{-# LANGUAGE DuplicateRecordFields #-}
module Lib
    ( register_chat
    ) where


import Prelude hiding (id)
import qualified Prelude as P
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Bitlbee
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad
import Chan
import Foreign.Marshal.Alloc (free)
import Data.List
import Text.HTML.TagSoup
import Data.List (foldl')

data ActiveThread = ActiveThread
    { groupChat    :: Ptr GroupChat
    , activeNumber :: ThreadNumber
    , activeBoard  :: String
    , lastMessage  :: Int
    , topicSet     :: Bool
    }


threadsVar :: IORef (TVar [ActiveThread])
{-# NOINLINE threadsVar #-}
threadsVar = unsafePerformIO (newTVarIO [] >>= newIORef)


foreign export ccall register_chat :: Ptr GroupChat -> CString -> CString -> IO ()
register_chat :: Ptr GroupChat -> CString -> CString -> IO ()
register_chat group number board = do
    number <- peekCString number
    board <- peekCString board
    let newActive = ActiveThread group (ThreadNumber . read $ number) board 0 False
    ts <- readIORef threadsVar
    atomically $ modifyTVar ts (\thrs -> newActive : thrs)
    sendMessageChat group "System" "Starting..." 0


-- | This function is called every 5 seconds. It check if there is a new message
-- pending in the API
foreign export ccall loop :: IO ()
loop = do
    ts <- readIORef threadsVar
    actives <- readTVarIO ts
    acs <- forM actives $ \t -> do
        posts' <- liftM (fmap posts) $ run (getThread (activeBoard t) (activeNumber t))
        case posts' of 
            Left err -> do
                sendMessageChat (groupChat t) "System" (show err) 0
                return t
            Right [] -> return t
            Right posts -> do
                let newPosts = dropWhile (\p -> noThread p <= lastMessage t) posts
                forM_ newPosts $ \p -> do
                    let user = case id p of
                                Nothing -> show $ noThread p
                                Just x -> x
                        msg = case com p of
                                Nothing -> "[EMPTY]"
                                Just str -> str
                        image = do
                                rename <- fmap show $ tim p
                                name   <- filename p
                                extension <- ext p
                                return $ "(https://i.4cdn.org/" ++ activeBoard t ++ "/" ++ rename ++ extension ++ " [" ++ name ++ "])"
                        msg' = case image of
                                Nothing -> msg
                                Just img -> img ++ " " ++ msg
                        user' = case country_name p of
                                    Nothing -> user
                                    Just code -> user ++ " [" ++ code ++ "]"
                    sendMessageChat (groupChat t) user' (stripTags msg') (time p)
                    return ()
                return t
                if length newPosts == 0
                    then return t
                    else return $ t { lastMessage = noThread . last $ newPosts }
    atomically $ writeTVar ts acs
    return ()


foreign export ccall threads_catalog :: Ptr IrcT -> CString -> IO ()
threads_catalog irct cBoard = do
    board <- peekCString cBoard
    result <- run (getCatalog board)
    case result of
        Left err -> do
            e <- newCString (show err)
            irc_rootmsg irct e
            free e
        Right cs -> do
            let catalog = mergeCatalogs cs
                ordered = reverse . sortBy (\x y -> compare (last_modified y) (last_modified x)) . threads $ catalog
                string  = unlines . map (\t -> show (noThreadCatalog t) ++ ": " ++ semantic_url t ++ " (R: " ++ (show $ replies t) ++ ", I: " ++ (show $ images t) ++ ")") $ ordered
            str <- newCString string
            irc_rootmsg irct str
            free str

stripTags :: String -> String
stripTags = foldl' (\acc x -> acc ++ renderTagsOptions renderOptions{optEscape = P.id} [x] ++ " ") "" . filter fn . parseTags
    where
        fn :: Tag String -> Bool
        fn (TagText _) = True
        fn _ = False
