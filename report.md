INDIVIDUAL PROJECT REPORT

Haskell Social Network Simulation

Student Name - Mannat Singh
Student ID - 250074794

INTRODUCTION - I have built a Haskell-based Social Network Simulation using concurrent threads to model real-time user interactions. This simulation creates multiple independent user agents that communicate via message passing, handling shared state and synchronization using Haskell's `MVar` primitives. This project has helped me verify the fundamentals of Functional Programming and concurrent system design in Haskell.

Methodology and Implementation -

a) Data Modelling and Types
The simulation relies on defining robust data structures. I created a `User` type that acts effectively as a concurrent object—holding not just static data (like the Username) but also mutable fields (`MVar`) for their Inbox and message counters.
For the messages, I defined a `Message` type that captures the content, sender, and a special flag (`msgIsViral`) to support the extension logic. By using these types, I ensured that data passing between threads is type-safe and structured.

b) Concurrency Architecture
I chose `MVar` (Mutable Variable) over other concurrency primitives (like STM) because it fits the "mailbox" model of a social network perfectly. Each user runs in their own lightweight thread (spawned via `forkIO`). They sleep for random intervals locally but access global state (the total message count) atomically. This mixed approach allows the system to be both asynchronous (users act independently) and synchronized (they all respect the global message limit).

The Extra Feature: Information Warfare Simulation
For the distinction part of the project, I decided to go beyond simple random messaging. The basic specification required random messages, but I felt that modeling a "real" social network required modeling the spread of information—specifically misinformation. I implemented an "Information Warfare" model with adversarial agents.

Influencers vs. Fact Checkers - Instead of all users being equal, I assigned them roles. "User1" acts as an Influencer who has a 50% chance of generating "Viral" content (marked with a 🔥 icon). On the other side, "User2" and "User3" act as Fact Checkers. If they receive viral content, they immediately identify and block it (stopping the chain), marked by a 🛑 icon. Standard users simply spread what they hear (⚡ icon).

Why this was challenging -

Strict Compliance vs. Graceful Shutdown - This was the most interesting design trade-off. A realistic system would use "Graceful Shutdown" to allow in-flight messages (like viral cascades) to complete. However, to adhere strictly to the assignment's termination criterion ("100 messages"), I implemented a **Hard Cap**. The system intentionally intercepts and drops in-flight viral propagation the moment the 100th message is logged. This demonstrates a conscious design trade-off: prioritizing specification compliance over the realism of completing every active cascade.

Ordering and Logs - Implementing the "numbered output" was tricky due to the nature of threads. If two threads print at the same time, lines can get scrambled. I solved this by using a dedicated atomic counter (`logIdx`) passed to every thread, ensuring that even though they run in parallel, their output lines (1., 2., 3...) remain perfectly sequential and readable for the marker.

How to Run the Application -
This project uses Stack for building and running the application. The following commands describe the key operations:

1. Build the Project - Compiles the source and downloads dependencies (random, base).
stack build

2. Run the Simulation - Launches the 10 threads and runs the simulation until 100 messages are reached.
stack run

3. Generate Documentation - Creates the HTML documentation from the Haddock comments.
stack haddock

Module Structure
Types.hs - Definitions of the concurrent data structures (`User`, `Message`).
Lib.hs - The core logic. Contains the `simulateUser` loop, the Influencer/Fact Checker logic, and the thread coordination code.
Main.hs - The entry point that initializes the global state and kicks off the simulation.
