module Bitlbee where


import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <bitlbee.h>

data GroupChat = GroupChat
data IrcT = IrcT
data ImConnection = ImConnection

foreign import ccall unsafe "imcb_chat_msg"
    imcb_chat_msg :: Ptr GroupChat -> CString -> CString -> CInt -> CInt -> IO ()

foreign import ccall unsafe "irc_rootmsg"
    irc_rootmsg :: Ptr IrcT -> CString -> IO ()



foreign import ccall unsafe "imcb_chat_topic"
    imcb_chat_topic :: Ptr GroupChat -> CString -> CString -> CInt -> IO ()



-- Haskell friendly functions

sendMessageChat :: Ptr GroupChat -> String -> String -> Int -> IO ()
sendMessageChat group user msg timestamp = do
  let ts = (fromIntegral timestamp) :: CInt
  msg' <- newCString msg
  user' <- newCString user
  imcb_chat_msg group user' msg' 0 ts
  free user'
  free msg'

setChatTopic :: Ptr GroupChat -> String -> String -> Int -> IO ()
setChatTopic group user topic timestamp = do
  let ts = (fromIntegral timestamp) :: CInt
  user' <- newCString user
  topic' <- newCString topic
  imcb_chat_topic group user' topic' ts
  free user'
  free topic'
