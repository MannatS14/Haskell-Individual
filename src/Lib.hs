module Lib (mainSimulation) where

import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Monad (forM_, forever, when)
import System.Random (randomRIO)
import Text.Printf (printf)
import Types

-- | The main entry point.
-- Setting up global state, creating users, spawning threads, and waiting for the simulation to finish.
mainSimulation :: IO ()
mainSimulation = do
    putStrLn "Starting Social Network Simulation..."
    putStrLn "--------------------------------------------------------"
    putStrLn "LOG LEGEND:"
    putStrLn "   User -> User   : Normal Message"
    putStrLn "   🔥 [VIRAL]     : Viral Message Initiated (High chance from User1)"
    putStrLn "   ⚡ [SPREAD]    : Viral Message Caught & Forwarded"
    putStrLn "   🛑 [BLOCK]     : Fact Checker (User2/3) Caught & Blocked"
    putStrLn "--------------------------------------------------------"
    
    -- Global counter for total messages sent across the network (Termination Condition)
    globalCount <- newMVar 0
    
    -- Log Index Counter (For sequential output numbering: 1, 2, 3...)
    logIdx <- newMVar 0
    
    -- Creating 10 users with unique names
    users <- mapM createUser [1..10]
    
    putStrLn $ "Created " ++ show (length users) ++ " users."
    
    -- Spawning a thread for each user
    mapM_ (forkIO . simulateUser users globalCount logIdx) users
    
    -- Waiting until the 100 message limit is reached
    waitForCompletion globalCount
    
    putStrLn "\nSimulation ended. Gathering the results..."
    printResults users

-- | Helper to print a message with a sequential index in '1. Message' format.
logWithIndex :: MVar Int -> String -> IO ()
logWithIndex indexMVar msg = do
    i <- modifyMVar indexMVar (\n -> return (n + 1, n + 1))
    putStrLn $ printf "%d. %s" i msg

-- | Creating a new User with empty mailboxes and counters.
createUser :: Int -> IO User
createUser i = do
    inbox <- newMVar []
    sent <- newMVar 0
    received <- newMVar 0
    return $ User
        { userName = "User" ++ show i
        , userInbox = inbox
        , userSentCount = sent
        , userReceivedCount = received
        }

-- | The logic that runs inside each User's thread.
simulateUser :: [User] -> MVar Int -> MVar Int -> User -> IO ()
simulateUser allUsers globalInfo logIdx me = forever $ do
    -- Sleep for a random interval (0.1s to 0.5s)
    delay <- randomRIO (100000, 500000)
    threadDelay delay
    
    -- Stopping if we've reached the message limit
    total <- readMVar globalInfo
    when (total < 100) $ do
        
        --  Picking a random recipient (excluding myself)
        let candidates = filter (\u -> userName u /= userName me) allUsers
        idx <- randomRIO (0, length candidates - 1)
        let target = candidates !! idx
        
        -- Creating the message
        let messages = ["Hello", "Hi there", "How are you?", "Haskell is fun", "Threads are cool", "Pink Elephants", "Blue Sky"]
        msgIdx <- randomRIO (0, length messages - 1)
        let body = messages !! msgIdx
        
        -- Extension logic - Influencer mechanism
        -- User1 is an Influencer with 50% viral chance, while others have 5%
        let viralProb = if userName me == "User1" then 0.5 else 0.05
        isViral <- (< viralProb) <$> (randomRIO (0.0, 1.0) :: IO Double)
        let msg = Message
                { msgContent = body -- Clean content without "(from User)"
                , msgSender = userName me
                , msgIsViral = isViral 
                }
        
        sendMessage target msg logIdx
        
        -- Updating local counters
        modifyMVar_ (userSentCount me) (\n -> return (n + 1))
        
        -- Updating the global counter
        modifyMVar_ globalInfo (\n -> do
            let newTotal = n + 1
            when (newTotal == 100) $ putStrLn "\n--- Limit of 100 messages reached! ---"
            return newTotal)
    
    -- Checking for any incoming viral messages to forward
    checkInbox allUsers me logIdx

-- | Thread-safe helper for sending a message to a user.
sendMessage :: User -> Message -> MVar Int -> IO ()
sendMessage target msg logIdx = do
    -- Updating target's state
    modifyMVar_ (userInbox target) $ \msgs -> return (msgs ++ [msg])
    modifyMVar_ (userReceivedCount target) $ \n -> return (n + 1)
    
    if msgIsViral msg
        then logWithIndex logIdx $ printf "[VIRAL] 🔥 %s sent to %s: %s" (msgSender msg) (userName target) (msgContent msg)
        else logWithIndex logIdx $ printf "  %s -> %s: %s" (msgSender msg) (userName target) (msgContent msg)

-- | Processing the inbox for any viral messages that needs to be forwarded.
checkInbox :: [User] -> User -> MVar Int -> IO ()
checkInbox allUsers me logIdx = do
    modifyMVar_ (userInbox me) $ \msgs -> do
        forM_ msgs $ \msg -> do
            when (msgIsViral msg) $ do
                if userName me `elem` ["User2", "User3"]
                    then do
                        -- Fact Checker Logic - Blocks the message!
                        logWithIndex logIdx $ printf "    [BLOCK] 🛑 %s stopped a VIRAL msg from %s" (userName me) (msgSender msg)
                    else do
                        -- Regular User - Spreads it!
                        logWithIndex logIdx $ printf "    [SPREAD] ⚡ %s caught VIRAL msg from %s -> Spreading..." (userName me) (msgSender msg)
                        spreadViralMessage allUsers me msg logIdx
        return [] -- Clearing the inbox

-- | Forwarding a message to two other random users.
spreadViralMessage :: [User] -> User -> Message -> MVar Int -> IO ()
spreadViralMessage allUsers me originalMsg logIdx = do
    let candidates = filter (\u -> userName u /= userName me) allUsers
    idx1 <- randomRIO (0, length candidates - 1)
    let target1 = candidates !! idx1
    
    let candidates2 = filter (\u -> userName u /= userName target1 && userName u /= userName me) allUsers
    idx2 <- randomRIO (0, length candidates2 - 1)
    let target2 = candidates2 !! idx2
    
    let forwardMsg = Message ("FWD: " ++ msgContent originalMsg) (userName me) False
    
    sendMessage target1 forwardMsg logIdx
    sendMessage target2 forwardMsg logIdx

-- | Waiting until the simulation is finished.
waitForCompletion :: MVar Int -> IO ()
waitForCompletion globalCounter = do
    count <- readMVar globalCounter
    when (count < 100) $ do
        threadDelay 100000 
        waitForCompletion globalCounter

-- | Displaying the final statistics table.
printResults :: [User] -> IO ()
printResults users = do
    putStrLn "\nUser Statistics:"
    putStrLn "Name\tSent\tReceived"
    putStrLn "----\t----\t--------"
    forM_ users $ \u -> do
        s <- readMVar (userSentCount u)
        r <- readMVar (userReceivedCount u)
        putStrLn $ printf "%s\t%d\t%d" (userName u) s r
