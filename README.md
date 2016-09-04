#SVG Path DSL - An Elm DSL for SVG Path elements

**Very experimental**


##ExampleStateful.elm

An api where the next action can depend on the current state  (implemenation in Monadic.elm) 


##Monadic.elm 

represents a diagram as `DrawState -> (appendable, DrawState)` (a pattern sometimes called the State monad). This thing has many nice properties from 
a category theory perspective, which I hope to look at at some point.

The basic design is to have a core sum type representing the SVG d primitives - called segment - and helpers to chain segments, thus building up more complicated structures. 
in the view, the `Diagram (List Segment)` is converted to a string. 

The small core means we can do all sorts of optimizations/manipulations at an abstract level - I'm thinking about merging two adjacent "M" instructions into one, for example. 
Only at the very last moment do strings come into play. 

With the DrawState we can also smartly combine diagrams. When combining diagrams A and B, the natural result is that B starts where A stops. But you could 
also let them both start at the same place, for instance. This sort of thing is very easy with this architecture.


##Path.elm 

a straight `List Segment -> String` conversion, where `Segment` is a sum type representing the SVG d primitives (see [here](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/))



##Internal.elm 

types and very primitive formatting functions

##ExampleNormal.elm 

testing ground for the Path.elm api


