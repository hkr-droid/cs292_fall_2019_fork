# Methodology

We will base our generated code on the notes and problems used in the UCSB CS170 Operating Systems class, which focuses on using semaphores for writing multithreaded code.     For the synthesis problem, we will design a DSL that describes the semantics of semaphores for synchronization problems. The DSL will be a subset of the target language that a user can synthesize code for.  With this DSL, and a user-provided specification of the synchronization scheme for the program, we will leverage a program synthesis framework like Neo or Rosette to synthesize the semaphore code.
