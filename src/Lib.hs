module Lib (mainSimulation) where

import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, readMVar)
import Control.Monad (forM_, when)
import Text.Printf (printf)
import Types
import UserLogic (simulateUser)

-- | The main entry point.
-- Setting up global state, creating users, spawning threads, and waiting for the simulation to finish.
mainSimulation :: IO ()
mainSimulation = do
    putStrLn "Starting Social Network Simulation..."
    putStrLn "--------------------------------------------------------"
    putStrLn "LOG LEGEND:"
    putStrLn "   User -> User   : Normal Message"
    putStrLn "    [VIRAL]     : Viral Message Initiated (High chance from User1)"
    putStrLn "    [SPREAD]    : Viral Message Caught & Forwarded"
    putStrLn "    [BLOCK]     : Fact Checker (User2/3) Caught & Blocked"
    putStrLn "--------------------------------------------------------"
    
    -- Global counter for total messages sent across the network (Termination Condition)
    globalCount <- newMVar 0
    
    -- Creating 10 users with unique names
    users <- mapM createUser [1..10]
    
    putStrLn $ "Created " ++ show (length users) ++ " users."
    
    -- Spawning a thread for each user
    mapM_ (forkIO . simulateUser users globalCount) users
    
    -- Waiting until the 100 message limit is reached
    waitForCompletion globalCount
    
    putStrLn "\nSimulation ended. Gathering the results..."
    printResults users

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
