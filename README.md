# salesperson: Comprehensive Collection of Functions for Solving and Analyzing the Travelling Salesperson Problem

[![Build Status](https://travis-ci.org/jakobbossek/salesperson.svg)](https://travis-ci.org/jakobbossek/salesperson)
[![Build status](https://ci.appveyor.com/api/projects/status/6hd5dguq6w3v7t7j/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/salesperson/branch/master)
[![Coverage Status](https://coveralls.io/repos/jakobbossek/salesperson/badge.svg?branch=master&service=github)](https://coveralls.io/github/jakobbossek/salesperson?branch=master))

## Description

The [Travelling Salesperson Problem](http://en.wikipedia.org/wiki/Travelling_salesman_problem) (TSP) is one of the most popular combinatorial optimization problems. Given a graph with pairwise distances between the nodes the problem is to find a shortest travelling tour which starts in a starting node, visits each node exactly once and returns back to the start. Despite its fairly easy problem formulation the TSP is unfortunately NP-hard and we do not know whether an exact deterministic polynomial time algorithm to solve it efficiently exists. However, in practise the TSP can be solved to optimality or at least close to optimality by means of sophisticated state-of-the-art heuristics, e.g., LKH or EAX.

These local search algorithms work well and partly theoretical results on their performance exist, but how they cope with problems at hand in practise is barely known. In the past decade a lot of scientific effort was spent on learning the actual behaviour of these algorithms.

Following the no-free-lunch theorem for optimization there is no single best solver, i.e., a solver which operates best on each problem instance. Instead the performance can vary significantly. It is therefore desirable to set up a portfolio of TSP solvers and - given a problem instance at hand - choose the solver which most probably will operate best on it within the scope of *Algorithm selection* (AC).

This package implements methods to compute characteristic properties, the socalled **(instance) features**, of TSP instances, e.g., the average edge costs, the angle between a node and both of his nearest neighbor nodes etc. This features can be used to fit performance models and apply machine learning algorithms in the AC context. The package furthermore offers an R interface to a set of - mostly inexact - TSP solvers. Currently the following algorithms can be applied:

* Repetitive Nearest-Neighbor
* {Nearest, Farthest, Cheapest, Arbitrary}-Insertion
* Minimum-Spanning-Tree Heuristic (also known as the 2-approximation algorithm)
* Christofides-heuristic (3/2-approximation)
* Exact CONCORDE solver
* LKH(-restart)
* EAX(-restart)

## Installation

Currently there is only this developement version of **salesperson**.
To install the current developement version of the package, install the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html) by Hadley Wickham, load it and type the following line to a R session:

```splus
install_github("jakobbossek/salesperson")
```

## Contact

Please address questions and missing features about the **salesperson package** to the maintainer Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/salesperson/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.
