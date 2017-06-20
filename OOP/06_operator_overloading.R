## ------------------------------------------------------------------------
modulus <- function(value, n) {
  result <- value %% n
  attr(result, "modulus") <- n
  class(result) <- c("modulus", class(value))
  result
}

## ------------------------------------------------------------------------
print.modulus <- function(x, ...) {
  cat("Modulus", attr(x, "modulus"), "values:\\n")
  # remove attributes to get plain numeric printing
  x <- unclass(x)
  attributes(x) <- NULL
  NextMethod()
}

(x <- modulus(1:6, 3))

## ------------------------------------------------------------------------
`+.modulus` <- function(x, y) {
  n <- attr(x, "modulus")
  x <- unclass(x)
  y <- unclass(y)
  modulus(x + y, n)
}

## ------------------------------------------------------------------------
x + 1:6
1:6 + x

## ------------------------------------------------------------------------
`+.modulus` <- function(x, y) {
  n <- ifelse(!is.null(attr(x, "modulus")),
              attr(x, "modulus"), attr(y, "modulus"))
  x <- unclass(x)
  y <- unclass(y)
  modulus(x + y, n)
}

x + 1:6
1:6 + x

## ------------------------------------------------------------------------
y <- modulus(1:6, 2)
x + y

## ------------------------------------------------------------------------
`+.modulus` <- function(x, y) {
  nx <- attr(x, "modulus")
  ny <- attr(y, "modulus")
  if (!is.null(nx) && !is.null(ny) && nx != ny)
    stop("Incompatible types")
  n <- ifelse(!is.null(nx), nx, ny)

  x <- unclass(x)
  y <- unclass(y)
  modulus(x + y, n)
}

x + y
y <- modulus(rev(1:6), 3)
x + y

## ------------------------------------------------------------------------
Ops.modulus <- function(e1, e2) {
  nx <- attr(e1, "modulus")
  ny <- attr(e2, "modulus")
  if (!is.null(nx) && !is.null(ny) && nx != ny)
    stop("Incompatible types")
  n <- ifelse(!is.null(nx), nx, ny)
  
  result <- unclass(NextMethod()) %% n
  modulus(result, n)
}

## ------------------------------------------------------------------------
y <- modulus(rev(1:6), 3)
x - y
x * y

## ------------------------------------------------------------------------
x == y
x == x
x != y
x != x

## ------------------------------------------------------------------------
- x

## ------------------------------------------------------------------------
Ops.modulus <- function(e1, e2) {
  nx <- attr(e1, "modulus")
  ny <- if (!missing(e2)) attr(e2, "modulus") else NULL
  if (!is.null(nx) && !is.null(ny) && nx != ny)
    stop("Incompatible types")
  n <- ifelse(!is.null(nx), nx, ny)
  
  result <- unclass(NextMethod()) %% n
  modulus(result, n)
}

- x

## ------------------------------------------------------------------------
symbolic_unit <- function(nominator, denominator = "") {
  non_empty <- function(x) x != ""
  nominator <- sort(Filter(non_empty, nominator))
  denominator <- sort(Filter(non_empty, denominator))
  structure(list(nominator = nominator, 
                 denominator = denominator),
            class = "symbolic_unit")
}

## ------------------------------------------------------------------------
as.character.symbolic_unit <- function(x, ...) {
  format_terms <- function(terms, op) {
    if (length(terms) == 0) return("1")
    paste0(terms, collapse = op)
  }
  nominator <- format_terms(x$nominator, "*")
  denominator <- format_terms(x$denominator, "/")
  paste(nominator, "/", denominator)
}

print.symbolic_unit <- function(x, ...) {
  cat(as.character(x, ...), "\\n")
}

(x <- symbolic_unit("m"))
(y <- symbolic_unit("m", "s"))

## ------------------------------------------------------------------------
`==.symbolic_unit` <- function(x, y) {
  if (!(inherits(x, "symbolic_unit") && 
      inherits(y, "symbolic_unit")))
      stop("Incompatible types")
  return(identical(x$nominator, y$nominator) && 
           identical(x$denominator, y$denominator))
}

`!=.symbolic_unit` <- function(x, y) !(x == y)

x == y
x != y

## ------------------------------------------------------------------------
`*.symbolic_unit` <- function(x, y) {
  symbolic_unit(c(x$nominator, y$nominator), 
                c(x$denominator, y$denominator))
}

`/.symbolic_unit` <- function(x, y) {
  symbolic_unit(c(x$nominator, y$denominator), 
                c(x$denominator, y$nominator))
}

x * y
x / y

## ------------------------------------------------------------------------
units <- function(value, nominator, denominator = "") {
  attr(value, "units") <- symbolic_unit(nominator, denominator)
  class(value) <- c("units", class(value))
  value
}

## ------------------------------------------------------------------------
print.units <- function(x, ...) {
  cat("Units: ", as.character(attr(x, "units")), "\\n")
  # remove attributes to get plain numeric printing
  x <- unclass(x)
  attributes(x) <- NULL
  NextMethod()
}

(x <- units(1:6, "m"))

## ------------------------------------------------------------------------
Ops.units <- function(e1, e2) {
  su1 <- attr(e1, "units")
  su2 <- if (!missing(e2)) attr(e2, "units") else NULL
  
  if (.Generic %in% c("+", "-", "==", "!=", 
                      "<", "<=", ">=", ">")) {
    if (!is.null(su1) && !is.null(su2) && su1 != su2)
      stop("Incompatible units")
    su <- ifelse(!is.null(su1), su1, su2)
    return(NextMethod())
  }

  if (.Generic == "*" || .Generic == "/") {
    if (is.null(su1))
      su1 <- symbolic_unit("")
    if (is.null(su2))
      su2 <- symbolic_unit("")
    su <- switch(.Generic, "*" = su1 * su2, "/" = su1 / su2)
    result <- NextMethod()
    attr(result, "units") <- su
    return(result)
  }
  
  # For the remaining operators we don't really have a good
  # way of treating the units so we strip that info and go
  # back to numeric values
  e1 <- unclass(e1)
  e2 <- unclass(e2)
  attributes(e1) <- attributes(e2) <- NULL
  NextMethod()
}

## ------------------------------------------------------------------------
2 * x
x + 2
x - 2

## ------------------------------------------------------------------------
(y <- units(1:6, "m", "s"))
x + y

## ------------------------------------------------------------------------
(z <- units(1:6, "m"))
x + z
x - z

## ------------------------------------------------------------------------
2 * x
x * y
x / y

