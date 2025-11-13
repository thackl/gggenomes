# Modify object class attributes

Set class of an object. Optionally append or prepend to exiting class
attributes. `add_class` is short for `set_class(x, class, "prepend")`.
`strip_class` removes matching class strings from the class attribute
vector.

## Usage

``` r
set_class(x, class, add = c("overwrite", "prepend", "append"))

add_class(x, class)

strip_class(x, class)
```

## Arguments

- x:

  Object to assign new class to.

- class:

  Class value to add/strip.

- add:

  Possible values: "overwrite", "prepend", "append"

## Value

Object x as class value.
