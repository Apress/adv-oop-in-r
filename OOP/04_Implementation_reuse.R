## ------------------------------------------------------------------------
A <- function() {
  structure(list(), class = "A")
}

B <- function() {
  structure(list(), class = c("B", "A"))
}

C <- function() {
  structure(list(), class = c("C", "B", "A"))
}

## ------------------------------------------------------------------------
x <- A()
y <- B()
z <- C()

class(x)
class(y)
class(z)

## ------------------------------------------------------------------------
f <- function(x) UseMethod("f")
f.default <- function(x) print("f.default")

## ------------------------------------------------------------------------
f(x)
f(y)
f(z)

## ------------------------------------------------------------------------
g <- function(x) UseMethod("g")
g.default <- function(x) print("g.default")
g.A <- function(x) print("g.A")

g(x)
g(y)
g(z)

## ------------------------------------------------------------------------
h <- function(x) UseMethod("h")
h.default <- function(x) print("h.default")
h.A <- function(x) print("h.A")
h.B <- function(x) print("h.B")

h(x)
h(y)
h(z)

## ------------------------------------------------------------------------
i <- function(x) UseMethod("i")
i.default <- function(x) print("i.default")
i.A <- function(x) print("i.A")
i.B <- function(x) print("i.B")
i.C <- function(x) print("i.C")

i(x)
i(y)
i(z)

## ------------------------------------------------------------------------
A <- function() {
  structure(list(), class = "A")
}

B <- function() {
  this <- A()
  class(this) <- c("B", class(this))
  this
}

C <- function() {
  this <- B()
  class(this) <- c("C", class(this))
  this
}

## ------------------------------------------------------------------------
f <- function(x) UseMethod("f")
f.default <- function(x) print("f.default")
f.A <- function(x) {
  print("f.A")
  NextMethod()
}
f.B <- function(x) {
  print("f.B")
  NextMethod()
}
f.C <- function(x) {
  print("f.C")
  NextMethod()
}

f(x)
f(y)
f(z)

## ------------------------------------------------------------------------
j <- function(x) UseMethod("j")
j.default <- function(x) print("j.default")
j.A <- function(x) {
  print("j.A")
  NextMethod()
}
j.C <- function(x) {
  print("j.C")
  NextMethod()
}

j(x)
j(y)
j(z)

## ------------------------------------------------------------------------
f(z)
class(z) <- rev(class(z))
f(z)

