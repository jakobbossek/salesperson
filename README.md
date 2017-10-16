# salesperson: Comprehensive Collection of Functions for Solving and Analyzing Travelling Salesperson Problems in R

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/salesperson)](http://cran.r-project.org/web/packages/salesperson)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/salesperson)](http://cran.rstudio.com/web/packages/salesperson/index.html)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/salesperson?color=orange)](http://cran.rstudio.com/web/packages/salesperson/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/salesperson.svg)](https://travis-ci.org/jakobbossek/salesperson)
[![Build status](https://ci.appveyor.com/api/projects/status/6hd5dguq6w3v7t7j/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/salesperson/branch/master)
[![Coverage Status](https://coveralls.io/repos/jakobbossek/salesperson/badge.svg?branch=master&service=github)](https://coveralls.io/github/jakobbossek/salesperson?branch=master)
[![Research software impact](http://depsy.org/api/package/cran/salesperson/badge.svg)](http://depsy.org/package/r/salesperson)

## Introduction

The [Travelling Salesperson Problem](http://en.wikipedia.org/wiki/Travelling_salesman_problem) (TSP) is one of the most popular combinatorial optimization problems. Given a graph with pairwise distances between the nodes the problem is to find a shortest travelling tour which starts in a starting node, visits each node exactly once and returns back to the start. Despite its fairly easy problem formulation the TSP is unfortunately NP-hard and we do not know whether an exact deterministic polynomial time algorithm to solve it efficiently exists. However, in practise the TSP can be solved to optimality or at least close to optimality by means of sophisticated state-of-the-art heuristics, e.g., LKH [1, 2] or EAX [5, 6].

These local search algorithms work well and partly theoretical results on their performance exist, but how they cope with problems at hand in practise is barely known. In the past decade a lot of scientific effort was spent on learning the actual behaviour of these algorithms.

Following the no-free-lunch theorem for optimization there is no single best solver, i.e., a solver which operates best on each problem instance. Instead the performance can vary significantly. It is therefore desirable to set up a portfolio of TSP solvers and - given a problem instance at hand - choose the solver which most probably will operate best on it within the scope of *Algorithm selection* (AC).

This package implements methods to compute characteristic properties, the socalled **(instance) features**, of TSP instances, e.g., the average edge costs, the angle between a node and both of his nearest neighbor nodes etc. This features can be used to fit performance models and apply machine learning algorithms in the AC context. The package furthermore offers an R interface to a set of - mostly inexact - TSP solvers. Currently the following algorithms can be applied.

Delegated to [TSP](https://cran.r-project.org/package=TSP) package:
* Repetitive Nearest-Neighbor
* {Nearest, Farthest, Cheapest, Arbitrary}-Insertion
* Exact [CONCORDE](http://www.math.uwaterloo.ca/tsp/concorde.html) solver [3]
* 2-Opt

The following two solvers are the current state-of-the-art in inexact TSP solving (see [1]). However, both are available for research only and need to be requested. 
* LKH+restart [1, 2, 4]
* EAX+restart [4, 5, 6]

[1] Helsgaun, K. (2000). An effective implementation of the lin-kernighan traveling salesman heuristic. European Journal of Operational Research, 126:106–130.

[2] Helsgaun, K. (2009). General k-opt submoves for the LinKernighan TSP heuristic. Mathematical Programming Computation, 1(2-3):119-163.

[3] Applegate, D. L., Bixby, R. E., Chvatal, V., and Cook, W. J. (2007). The Traveling Salesman Problem: A Computational Study. Princeton University Press, Princeton, NJ, USA.

[4] Kotthoff, L., Kerschke, P., Hoos, H., and Trautmann, H. (2015). Improving the State of the Art in Inexact TSP Solving using Per-Instance Algorithm Selection. In LION 9, pages 202-217.

[5] Nagata, Y. and Kobayashi, S. (1997). Edge assembly crossover: A high-power genetic algorithm for the travelling salesman problem. In Bäck, T., editor, Proceedings of the Seventh International Conference on Genetic Algorithms (ICGA97), pages 450-457, San Francisco, CA. Morgan Kaufmann.

[6] Nagata, Y. and Kobayashi, S. (2013). A powerful genetic algorithm using edge assembly crossover for the traveling salesman problem. INFORMS Journal on Computing, 25(2):346-363.

## Example

As a simple example we run the nearest-neighbour heuristic on a randomly genrated clustered TSP instance with 300 nodes distributed among three clusters and print/plot the result.
```r
library(salesperson)
library(ggplot2)

set.seed(1)

x = generateClusteredNetwork(n.points = 200L, n.cluster = 3L)
res = runSolver("nn", x)
print(res)
print(autoplot(x, path = res$tour, close.path = TRUE))
```

In order to run the exact CONCORDE TSP solver we first need to [download the executable](http://www.math.uwaterloo.ca/tsp/concorde/downloads/downloads.htm) for our operating system. Say, the path to the executable is */path/to/concorde* (on a linux system) or *C:/path/to/concorde* (on a windows system). We need to pass this path to `runSolver` in order to call CONCORDE.
```r
res = runSolver("concorde", x, solver.path = "/Users/you/concorde")
print(res)
print(autoplot(x, path = res$tour, close.path = TRUE))
```

## Installation

Currently there is only this developement version of **salesperson**.
To install the current developement version of the package, install the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html) by Hadley Wickham, load it and type the following line to a R session:

```splus
devtools::install_github("jakobbossek/salesperson")
```

## Contributing to salesperson

Please address questions and missing features about the **salesperson package** to the maintainer Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/salesperson/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.

Contributions to this software package are welcome via [pull requests](https://help.github.com/articles/about-pull-requests/) and will be merged at the sole discretion of the author.

## Related R Packages

* The [TSP](https://cran.r-project.org/package=TSP) package offers an interface to sevaral heuristics and the Concorde solver. All its methods are interfaced by salesperson.

* The TSP may be tackled by evolutionary algorithms. Thus, packages [ecr](https://cran.r-project.org/package=ecr) or [GA](https://cran.r-project.org/package=GA) may be used to solve the problem.

* Given an adequate formulation as an integer linear program (ILP) the TSP may be solved using, e.g., the [lpSolve](https://cran.r-project.org/web/packages/lpSolve/index.html) package.
