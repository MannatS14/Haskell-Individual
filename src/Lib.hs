module Lib (mainSimulation) where

import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forM_, forever, when)
import System.Random (randomRIO)
import Text.Printf (printf)
import Types

-- | The main entry point for the simulation.
-- 1. Creates a shared global counter.
-- 2. Creates 10 user definitions.
-- 3. Spawns a background thread for each user.
-- 4. Waits until 100 messages are sent.
-- 5. Prints the final statistics.
mainSimulation :: IO ()
mainSimulation = do
    putStrLn "Starting Social Network Simulation..."
    
    -- Global counter for total messages sent across the network
    globalCount <- newMVar 0
    
    -- Create 10 users with unique names
    users <- mapM createUser [1..10]
    
    putStrLn $ "Created " ++ show (length users) ++ " users."
    
    -- Spawn threads: Each user gets their own lightweight thread
    mapM_ (forkIO . simulateUser users globalCount) users
    
    -- Main thread waits for the simulation to complete
    waitForCompletion globalCount
    
    -- Print the report
    putStrLn "\nSimulation ended. Gathering results..."
    printResults users

-- | Creates a new User with empty mailboxes and counters.
createUser :: Int -> IO User
createUser i = do
    inbox <- newMVar []  -- Empty inbox
    sent <- newMVar 0
    received <- newMVar 0
    return $ User
        { userName = "User" ++ show i
        , userInbox = inbox
        , userSentCount = sent
        , userReceivedCount = received
        }

-- | The logic that runs inside each User's thread.
simulateUser :: [User] -> MVar Int -> User -> IO ()
simulateUser allUsers globalInfo me = forever $ do
    -- 1. Simulate "working" or "thinking" time (0.1s to 0.5s)
    delay <- randomRIO (100000, 500000)
    threadDelay delay
    
    -- 2. Check if the simulation is still active
    total <- readMVar globalInfo
    when (total < 100) $ do
        
        -- 3. Pick a random recipient (not myself)
        let candidates = filter (\u -> userName u /= userName me) allUsers
        idx <- randomRIO (0, length candidates - 1)
        let target = candidates !! idx
        
        -- 4. Create a message
        -- EXTENSION: 5% chance to send a "Viral" message
        isViral <- (< 0.05) <$> (randomRIO (0.0, 1.0) :: IO Double)
        let msg = Message
                { msgContent = "Hello from " ++ userName me
                , msgSender = userName me
                , msgIsViral = isViral
                }
        
        -- 5. Send the message
        sendMessage target msg
        
        -- 6. Update my 'Sent' counter
        modifyMVar_ (userSentCount me) (\n -> return (n + 1))
        
        -- 7. Update the global counter
        modifyMVar_ globalInfo (\n -> do
            let newTotal = n + 1
            when (newTotal == 100) $ putStrLn "\n--- Limit of 100 messages reached! ---"
            return newTotal)
    
    -- 8. Check my inbox for any incoming Viral messages to forward
    checkInbox allUsers me

-- | Helper to send a message to a user.
-- Thread-safe: modifies the target's inbox MVar.
sendMessage :: User -> Message -> IO ()
sendMessage target msg = do
    -- Add message to inbox
    modifyMVar_ (userInbox target) $ \msgs -> return (msgs ++ [msg])
    -- Increment their received count
    modifyMVar_ (userReceivedCount target) $ \n -> return (n + 1)
    
    let viralTag = if msgIsViral msg then " [VIRAL]" else ""
    putStrLn $ printf "%s -> %s: %s%s" (msgSender msg) (userName target) (msgContent msg) viralTag

-- | Checks the user's inbox for viral messages and forwards them.
-- Clears the inbox after processing.
checkInbox :: [User] -> User -> IO ()
checkInbox allUsers me = do
    modifyMVar_ (userInbox me) $ \msgs -> do
        forM_ msgs $ \msg -> do
            when (msgIsViral msg) $ do
                -- It's viral! Forward to 2 random people
                -- (Implementation Note: Forwarded copies are not marked Viral 
                -- to prevent infinite cascades in this small simulation)
                putStrLn $ printf "!!! %s caught a VIRAL message from %s! Spreading it..." (userName me) (msgSender msg)
                spreadViralMessage allUsers me msg
        return [] -- Clear inbox

-- | Logic to forward a message to 2 random users.
spreadViralMessage :: [User] -> User -> Message -> IO ()
spreadViralMessage allUsers me originalMsg = do
    -- Pick 2 distinct targets
    let candidates = filter (\u -> userName u /= userName me) allUsers
    idx1 <- randomRIO (0, length candidates - 1)
    let target1 = candidates !! idx1
    
    let candidates2 = filter (\u -> userName u /= userName target1 && userName u /= userName me) allUsers
    idx2 <- randomRIO (0, length candidates2 - 1)
    let target2 = candidates2 !! idx2
    
    let forwardMsg = Message ("FWD: " ++ msgContent originalMsg) (userName me) False
    
    sendMessage target1 forwardMsg
    sendMessage target2 forwardMsg

-- | Busy-wait until the global counter reaches 100
waitForCompletion :: MVar Int -> IO ()
waitForCompletion globalCounter = do
    count <- readMVar globalCounter
    when (count < 100) $ do
        threadDelay 100000 -- Check every 0.1s
        waitForCompletion globalCounter

-- | Print the final table of results
printResults :: [User] -> IO ()
printResults users = do
    putStrLn "\nUser Statistics:"
    putStrLn "Name\tSent\tReceived"
    putStrLn "----\t----\t--------"
    forM_ users $ \u -> do
        s <- readMVar (userSentCount u)
        r <- readMVar (userReceivedCount u)
        putStrLn $ printf "%s\t%d\t%d" (userName u) s r
