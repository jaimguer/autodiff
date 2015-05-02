# Introduction


# Overview of Autodifferentiation


# Overview of Haskell Implementation
The bulk of this code comes from Jerzy Karczmarczuk's Haskell implementation.  In his paper, he discusses the usefullness of Haskell's lazy evaluation in constructing a system that uses it.  However, the first part of the paper works perfectly well in a strict setting.  The general idea is to create an algebra of dual numbers.  In the algebra of duals, the first component can be understood to be a function, while the second component is the derivative of that function.  In order to actually calculate the derivative, a $\textit{dlift} function$ is used in conjuction with a point (that has actually been coerced into a dual).


# Overview of Scheme and Lua Implementations
Operator overloading via explicit renaming
Operator overloading via metatables

# Example

# Thoughts
## Operator overloading is DSL construction
Hudak's paper?
At the highest level, we can understand operator overloading as a way to implement a DSL.  This is particularly useful in our case, where both implementations (more or less) retain their mathematical flavor.

## Derivative construction is metaprogramming

## Optimizations to derivative calculations


