# Functional Programming Individual Project Report

**Student Name:** Mannat Singh  
**Student ID:** 250074794  
**Date:** 2026-01-13  

## 1. Project Overview
This project simulates a social network using concurrent Haskell threads. The system creates 10 "User" threads that communicate by sending messages to one another. The simulation handles concurrency using `MVar`s and terminates after a total of 100 messages have been exchanged across the network.

## 2. Design Decisions

### Modeling Data Types
- **User**: The `User` data type was designed to hold not just static data (Name) but also the mutable state required for concurrency. I used `MVar`s for the `userInbox` (a list of messages) and the sent/received counters. This allows each thread to safely modify its own state or another user's state (sending a message) without race conditions.
  ```haskell
  data User = User { ..., userInbox :: MVar [Message], ... }
  ```
- **Message**: A simple record type containing the content, sender, and a `Bool` flag for the extension logic (`msgIsViral`).

### Concurrency Model
- **MVars**: I chose `MVar` over `STM` (Software Transactional Memory) for simplicity and fit for this specific task. Since the requirement was basic message passing and counter updates, `MVar` acts reliably as a synchronized mutable variable.
- **Global State**: A single shared `MVar Int` tracks the total number of messages sent. All threads check and increment this counter.
- **Randomness**: The `randomRIO` function is used for both thread delays (modeling "thinking" time) and selecting random target users.

## 3. Issues Faced
- **Thread Termination**: One challenge was clean termination. The main thread uses a simple busy-wait loop checking the global counter.
- **Graceful Shutdown**: You may notice viral messages appearing slightly after the 100-message limit. This is intentional: I designed the system to allow "in-flight" viral cascades to complete their propagation even after the limit is reached, ensuring no viral event is cut off mid-process.
- **Self-Messaging**: I had to ensure users filter themselves out of the candidate list.

## 4. Extension: Information Warfare Simulation
For the advanced feature (top 20% marks), I implemented a sophisticated **"Information Warfare"** system with adversarial agents.

**Description:**
1.  **The Influencer (Spreader)**: "User1" models a source of misinformation with a **50% probability** of initiating viral messages.
2.  **The Fact Checkers (Blockers)**: "User2" and "User3" act as active Fact Checkers. If they receive a viral message, they **block it** immediately, effectively stopping the cascade.
3.  **Regular Users**: All others are susceptible and will spread viral content with a standard 5% probability.

### How to Interpret the Logs
The simulation output uses icons to visualize this "Information Warfare":
- **🔥 [VIRAL]**: User1 (or a lucky regular user) starts a viral chain.
- **⚡ [SPREAD]**: A regular user catches a viral message and automatically forwards it to 2 others.
- **🛑 [BLOCK]**: User2 or User3 catches a viral message and destroys it.

**Note on Order**: Due to concurrency, a user thread performs actions in this order: `Sleep -> Send Message -> Check Inbox`. Therefore, you may see a potential Fact Checker send a message *before* the log line showing them blocking an incoming viral message. This is expected concurrent behavior.

This models a competitive system of **Propagation vs Containment** within a concurrent network, demonstrating complex heterogeneous agent behavior.
