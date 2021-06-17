# five-finger-exercises

This directory contains examples of code I have written in various
languages.  Some of this stuff works directly on downloading, some
not. It is provided for illustrative purposes rather than directly for
use.

 - CombinatorParser.R: A minimal monadic parser for R

 - HO: A Julia package for generating optimal cross-hub schedules for
   the combined fleets of an airline group, using a multi-agent
   simulation, with approximate dynamic programming / reinforcement
   learning.

 - HiearchicalRegressionDemo.R: a demonstration I built for teaching /
   illustrating  how hierarchical  regression  works, specifically  in
   Stan.  The script is in R.

 - OptimalRouting.jl: A small demo using the Cbc mixed integer
   optimisation package from inside Julia. Solves the delivery truck
   fleet optimal routing problem.

 - ResourcePlanningDemo.hs: An implementation in Haskell of the blood
   bank problem from Powell, Approximate Dynamic Programming. There is
   some original work here - this shows how to extend an arbitrary
   mixed-integer model with (low dimensional) convex expectation
   functions for the purposes of tackling multi-period / multi-echelon
   problems.

 - RiskAccounting.pl: A complete - production scale - reconstruction
   of the ETL for risk accounting for a large bank, written in Prolog.
   I built this for testing purposes for a bank that was trying to
   merge two such systems (it started as a personal attempt to
   understand the accounting logic).  The orignal system for which
   this is a reference model was written in Oracle and ran overnight
   on enterprise hardware. My reconstruction ran in 20 minutes on my
   laptop.  My reconstruction not only systematically verifed all the
   aggregation logic in the system (which had never previously been
   done), but was also able to confirm, for the first time, that the
   core four rule model for the accounting aggregation was correct.

 - Sudoko.py: A solver for Sudoko problems in python - pleasingly
   compact, I think.

 - pentominos.jl: implementation in Julia of Knuth's dancing links /
   Algorithm X solution for the exact cover problem.  Set up currently
   to enumerate complete solution sets for pentomino problems, which
   it does very fast.

