module Types (Message(..), User(..)) where

import Control.Concurrent.MVar (MVar)

-- | A message sent between users in the social network.
data Message = Message
    { msgContent :: String -- ^ The actual text content of the message.
    , msgSender  :: String -- ^ The name of the user who sent the message.
    , msgIsViral :: Bool   -- ^ Extension: If True, the receiver will forward this message.
    } deriving (Show, Eq)

-- | A user in the social network simulation.
data User = User
    { userName          :: String          -- ^ The user's name (e.g., "User1").
    , userInbox         :: MVar [Message]  -- ^ Thread-safe inbox to store received messages.
    , userSentCount     :: MVar Int        -- ^ Counter for total messages sent.
    , userReceivedCount :: MVar Int        -- ^ Counter for total messages received.
    }
