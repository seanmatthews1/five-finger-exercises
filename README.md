# five-finger-exercises

This directory contains examples of code I have written in various
languages.  Some of this stuff works directly on downloading, some
not. It is provided for illustrative purposes rather than directly for
use.

 - CombinatorParser.R: A minimal monadic parser for R

 - HO: A Julia package for generating optimal cross-hub schedules for
   the combined fleets of an airline group, using a Multi-agent
   simulation, with approximate dynamic programming / reinforcement
   learning.

 - HiearchicalRegressionDemo.R: a demonstration I constructed for
   teaching / illustrating how hierarchical regression works.  The
   script here is in R, while the inlined model code is in Stan.

 - OptimalRouting.jl: A small demo using the Cbc mixed integer
   optimisation package from inside Julia. Solves the delivery truck
   fleet optimal routing problem.

 - ResourcePlanningDemo.hs: An implementation in Haskell of the blood
   bank problem from Powell, Approximate Dynamic Programming. There is
   some original work here - this shows how to extend an arbitrary
   mixed-integer model with (low dimensional) convex expectation
   functions for the purposes of tackling multi-period / multi-echelon
   problems.

 - Sudoko.py: A solver for Sudoko problems in python - pleasingly
   compact, I think.

 - pentominos.jl: implementation in Julia of Knuth's dancing links /
   Algorithm X solution for the exact cover problem.  Set up currently
   to enumerate complets solution sets for pentomino problems, which
   is does extremely fast.
