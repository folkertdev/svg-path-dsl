#SVG Path DSL - An Elm DSL for SVG Path elements

**Very experimental**

##Path.elm 

a straight `List Segment -> String` conversion, where `Segment` is a sum type representing the SVG d primitives (see [here](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/))

##Monadic.elm 

a `List (Segment -> DrawState ->  ( String, DrawState) ) -> DrawState -> String` conversion. The `DrawState` holds the current location of the 
cursor and the origin. This API has a lot more abilities, but is a bit more complex (in the types). 


##Internal.elm 

types and very primitive formatting functions

##Example.elm 

testing ground for the Path.elm api

