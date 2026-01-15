module Types (Message(..), User(..)) where

import Control.Concurrent.MVar (MVar)

-- | A message sent between users in the social network.
data Message = Message
    { msgContent :: String -- ^ The message text.
    , msgSender  :: String -- ^ The sender's username.
    , msgIsViral :: Bool   -- ^ Extension: Receiver forwards this if True.
    } deriving (Show, Eq)

-- | A user in the social network simulation.
data User = User
    { userName          :: String          -- ^ The user's name.
    , userInbox         :: MVar [Message]  -- ^ Thread-safe inbox for storing received messages.
    , userSentCount     :: MVar Int        -- ^ Keeping track of total messages sent.
    , userReceivedCount :: MVar Int        -- ^ Keeping track of total messages received.
    }
