module UserLogic (simulateUser, sendMessage, checkInbox, spreadViralMessage) where

import Control.Concurrent (threadDelay, MVar, readMVar, modifyMVar, modifyMVar_)
import Control.Monad (forM_, forever, when)
import System.Random (randomRIO)
import Text.Printf (printf)
import Types

-- | The core behaviour loop for a single User.
simulateUser :: [User] -> MVar Int -> User -> IO ()
simulateUser allUsers globalInfo me = forever $ do
    delay <- randomRIO (100000, 500000)
    threadDelay delay
    
    total <- readMVar globalInfo
    when (total < 100) $ do
        let candidates = filter (\u -> userName u /= userName me) allUsers
        idx <- randomRIO (0, length candidates - 1)
        let target = candidates !! idx
        
        let messages = ["Hello", "Hi there", "How are you?", "Haskell is fun", "Threads are cool", "Pink Elephants", "Blue Sky"]
        msgIdx <- randomRIO (0, length messages - 1)
        let body = messages !! msgIdx
        
        let viralProb = if userName me == "User1" then 0.5 else 0.05
        isViral <- (< viralProb) <$> (randomRIO (0.0, 1.0) :: IO Double)
        let msg = Message
                { msgContent = body
                , msgSender = userName me
                , msgIsViral = isViral 
                }
        
        sendMessage target msg globalInfo
        
        -- Updating local sent counter
        modifyMVar_ (userSentCount me) (\n -> return (n + 1))
    
    checkInbox allUsers me globalInfo

-- | Helper to safely send a message.
-- Performing the checking, updating, and printing inside the global lock to ensure sequential output.
sendMessage :: User -> Message -> MVar Int -> IO ()
sendMessage target msg globalInfo = do
    modifyMVar_ globalInfo $ \n -> do
        if n >= 100
            then return n -- Limit reached, do nothing.
            else do
                let currentIndex = n + 1
                
                -- Delivering to inbox (This takes a brief lock on the target's inbox)
                modifyMVar_ (userInbox target) $ \msgs -> return (msgs ++ [msg])
                modifyMVar_ (userReceivedCount target) $ \c -> return (c + 1)
                
                -- Printing while holding the global lock.
                -- This guarantees that "11" finishes printing before anyone else can grab "12".
                if msgIsViral msg
                    then putStrLn $ printf "%d.  [VIRAL]  %s sent to %s: %s" currentIndex (msgSender msg) (userName target) (msgContent msg)
                    else putStrLn $ printf "%d.  %s -> %s: %s" currentIndex (msgSender msg) (userName target) (msgContent msg)
                
                when (currentIndex == 100) $ putStrLn "\n--- Limit of 100 messages reached! ---"
                
                return currentIndex

-- | Checking inbox.
-- CRITICAL - Extracting the messages first, then processing them outside the lock to avoid deadlock.
checkInbox :: [User] -> User -> MVar Int -> IO ()
checkInbox allUsers me globalInfo = do
    -- 1. Atomically cleaing inbox and retrieving messages
    messages <- modifyMVar (userInbox me) $ \msgs -> return ([], msgs)
    
    -- 2. Processing messages (No inbox lock held here)
    forM_ messages $ \msg -> do
        when (msgIsViral msg) $ do
            if userName me `elem` ["User2", "User3"]
                then do
                    putStrLn $ printf "      [BLOCK]  %s stopped a VIRAL msg from %s" (userName me) (msgSender msg)
                else do
                    -- Spreading requires calling sendMessage, which takes the Global Lock.
                    -- Doing this without holding the Inbox Lock (step 1) is safe.
                    val <- readMVar globalInfo
                    if val >= 100 
                        then putStrLn $ printf "      [HALTED]  %s caught VIRAL msg but limit reached -> Stopped." (userName me)
                        else do
                            putStrLn $ printf "      [SPREAD]  %s caught VIRAL msg from %s -> Spreading..." (userName me) (msgSender msg)
                            spreadViralMessage allUsers me msg globalInfo

-- | Spreading a viral message.
spreadViralMessage :: [User] -> User -> Message -> MVar Int -> IO ()
spreadViralMessage allUsers me originalMsg globalInfo = do
    let candidates = filter (\u -> userName u /= userName me) allUsers
    idx1 <- randomRIO (0, length candidates - 1)
    let target1 = candidates !! idx1
    
    let candidates2 = filter (\u -> userName u /= userName target1 && userName u /= userName me) allUsers
    idx2 <- randomRIO (0, length candidates2 - 1)
    let target2 = candidates2 !! idx2
    
    let forwardMsg = Message ("FWD: " ++ msgContent originalMsg) (userName me) False
    
    sendMessage target1 forwardMsg globalInfo
    sendMessage target2 forwardMsg globalInfo
