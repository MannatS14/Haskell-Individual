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
- **Thread Termination**: One challenge was clean termination. The main thread uses a simple busy-wait loop checking the global counter. In a production system, I would use an `Async` library or a `WaitSema`, but for this assignment, checking the `MVar` status effectively simulates the requirement to stop at 100 messages.
- **Self-Messaging**: I had to ensure users filter themselves out of the candidate list so they don't send messages to themselves.

## 4. Extension: Viral Messages
For the advanced feature (top 20% marks), I implemented a **"Viral Message"** system.

**Description:**
When a user prepares to send a message, there is a 5% chance it will be marked as **Viral** (`msgIsViral = True`).
Logic:
1.  When a User thread checks its inbox and finds a message marked `isViral`, it triggers a `spreadViralMessage` function.
2.  This function immediately forwards the content to **two other random users**.
3.  To prevent an infinite explosion of messages in this small simulation, the forwarded copies are *not* marked viral, but the "fan-out" effect is clearly visible in the logs.

This extension demonstrates dynamic behavior where one event triggers multiple concurrent side-effects.
