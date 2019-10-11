**Please keep in mind that this project is from fall 2017.**
## Ocaml projects 

This repository contains the solutions to the programming assignments for the Fall 2017 Introduction to Programming course at the University of Warsaw. 

The specification and comments is written in Polish. Here you can find brief descriptions of those projects

#### 1: Calculator for physicists
This project implements floating-point arithmetics for values with an error allowance. Say, we measure speed as 5 km/h +- 0.1km/h. We would like to see how this uncertainty of measurement affects other variables.

The solution treats these values as an interval of possible values and allows to perform addition, subtraction, multiplication and division of these values. It supports non-numeric types, like NaN, +-infinity and operations including 0.

#### 2: Leftist tree
This project implements [leftist trees](https://en.wikipedia.org/wiki/Leftist_tree).
#### 3: Tree modifications
This project is a modification of a polymorphic set implementation to store integer numbers as intervals.
#### 4: Origami
An origami simulator, allowing to check at how many points the original card is pierced.
#### 5: Topological sort
Sorts a [directed acyclic graph.](https://en.wikipedia.org/wiki/Directed_acyclic_graph)
#### 6: Pouring water
There are n glasses of water, each with capacity x1, x2, ..., xn. Initially all glasses are empty. In each move you can either:
* Fill the glass to the top, or
* Remove all water from a selected glass, or
* Pour water from one glass to another.
The program uses the **backtrack** algorithm to determine the minimum number of moves (or output -1, if impossible) to reach the state (y1, y2, ..., yn) for given y1, ..., yn.
