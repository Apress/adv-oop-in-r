## ------------------------------------------------------------------------
top <- function(stack) UseMethod("top")
pop <- function(stack) UseMethod("pop")
push <- function(stack, element) UseMethod("push")
is_empty <- function(stack) UseMethod("is_empty")

## ------------------------------------------------------------------------
top.default <- function(stack) .NotYetImplemented()
pop.default <- function(stack) .NotYetImplemented()
push.default <- function(stack, element) .NotYetImplemented()
is_empty.default <- function(stack) .NotYetImplemented()

## ------------------------------------------------------------------------
empty_vector_stack <- function() {
  stack <- vector("numeric")
  class(stack) <- "vector_stack"
  stack
}

stack <- empty_vector_stack()
stack
attributes(stack)
class(stack)

## ------------------------------------------------------------------------
top.vector_stack <- function(stack) stack[1]
pop.vector_stack <- function(stack) {
  new_stack <- stack[-1]
  class(new_stack) <- "vector_stack"
  new_stack
}
push.vector_stack <- function(element, stack) {
  new_stack <- c(element, stack)
  class(new_stack) <- "vector_stack"
  new_stack
}
is_empty.vector_stack <- function(stack) length(stack) == 0

## ------------------------------------------------------------------------
stack <- push(stack, 1)
stack <- push(stack, 2)
stack <- push(stack, 3)
stack

## ------------------------------------------------------------------------
while (!is_empty(stack)) {
  stack <- pop(stack)
}

## ------------------------------------------------------------------------
make_vector_stack <- function(elements) {
  structure(elements, class = "vector_stack")
}
empty_vector_stack <- function() {
  make_vector_stack(vector("numeric"))
}
top.vector_stack <- function(stack) stack[1]
pop.vector_stack <- function(stack) {
  make_vector_stack(stack[-1])
}
push.vector_stack <- function(stack, element) {
  make_vector_stack(c(element, stack))
}
is_empty.vector_stack <- function(stack) length(stack) == 0

## ------------------------------------------------------------------------
make_list_node <- function(head, tail) {
  list(head = head, tail = tail)
}
make_list_stack <- function(elements) {
  structure(list(elements = elements), class = "list_stack")
}
empty_list_stack <- function() make_list_stack(NULL)
top.list_stack <- function(stack) stack$elements$head
pop.list_stack <- function(stack) make_list_stack(stack$elements$tail)
push.list_stack <- function(stack, element) {
  make_list_stack(make_list_node(element, stack$elements))
}
is_empty.list_stack <- function(stack) is.null(stack$elements)

stack <- empty_list_stack()
stack <- push(stack, 1)
stack <- push(stack, 2)
stack <- push(stack, 3)
stack

## ------------------------------------------------------------------------
stack_reverse <- function(empty, elements) {
  stack <- empty
  for (element in elements) {
    stack <- push(stack, element)
  }
  result <- vector(class(top(stack)), length(elements))
  for (i in seq_along(result)) {
    result[i] <- top(stack)
    stack <- pop(stack)
  }
  result
}

stack_reverse(empty_vector_stack(), 1:5)
stack_reverse(empty_list_stack(), 1:5)

## ----microbenchmark_stacks, cache=TRUE-----------------------------------
library(microbenchmark)
microbenchmark(stack_reverse(empty_vector_stack(), 1:10),
               stack_reverse(empty_list_stack(), 1:10))
microbenchmark(stack_reverse(empty_vector_stack(), 1:1000),
               stack_reverse(empty_list_stack(), 1:1000))

## ----performance_plotting_stacks, cache=TRUE, echo=FALSE-----------------
library(tibble)
get_time <- function(empty, n) 
  microbenchmark(stack_reverse(empty, 1:n))$time
time_stacks <- function(n) {
  rbind(tibble(Implementation = "Vector", n = n, 
               Time = get_time(empty_vector_stack(), n)),
        tibble(Implementation = "List", n = n, 
               Time = get_time(empty_list_stack(), n)))
  
}
times <- do.call(rbind, 
                 lapply(seq(100, 5000, length.out = 10), 
                        time_stacks))

## ----performance_plot, fig.cap="Time usage of reversal with two different stacks.", echo=FALSE----
library(ggplot2)
ggplot(times) + 
  geom_boxplot(aes(x = as.factor(n), 
                   y = Time, 
                   fill = Implementation)) +
  scale_fill_grey() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom")

## ------------------------------------------------------------------------
pop_until <- function(stack, element) {
  if (element %in% stack) {
    while (top(stack) != element) stack <- pop(stack)
  }
  stack
}

library(magrittr)
vector_stack <- empty_vector_stack() %>%
  push(1) %>%
  push(2) %>%
  push(3) %T>% print
pop_until(vector_stack, 1)
pop_until(vector_stack, 5)

## ------------------------------------------------------------------------
pop_until <- function(stack, element) {
  s <- stack
  while (!is_empty(s) && top(s) != element) s <- pop(s)
  if (is_empty(s)) stack else s
}

## ------------------------------------------------------------------------
contains <- function(stack, element) {
  UseMethod("contains")
}
contains.default <- function(stack, element) {
  .NotYetImplemented()
}
contains.vector_stack <- function(stack, element) {
  element %in% stack
}

## ------------------------------------------------------------------------
merge_lists <- function(x, y) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)
  
  if (x[[1]] < y[[1]]) {
    c(x[1], merge_lists(x[-1], y))
  } else {
    c(y[1], merge_lists(x, y[-1]))
  }
}

sort_list <- function(x) {
  if (length(x) <= 1) return(x)
  
  start <- 1
  end <- length(x)
  middle <- end %/% 2
  
  merge_lists(sort_list(x[start:middle]), 
              sort_list(x[(middle+1):end]))
}

## ------------------------------------------------------------------------
merge_lists <- function(x, y) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)

  i <- j <- k <- 1
  n <- length(x) + length(y)
  result <- vector("list", length = n)  

  while (i <= length(x) && j <= length(y)) {
    if (x[[i]] < y[[j]]) {
      result[[k]] <- x[[i]]
      i <- i + 1
    } else {
      result[[k]] <- y[[j]]
      j <- j + 1
    }
    k <- k + 1
  }
  
  if (i > length(x)) {
    result[k:n] <- y[j:length(y)]
  } else {
    result[k:n] <- x[i:length(x)]
  }
  
  result
}

## ------------------------------------------------------------------------
make_tuple <- function(x, y) {
  result <- c(x,y)
  class(result) <- "tuple"
  result
}

x <- list(make_tuple(1,2),
          make_tuple(1,1),
          make_tuple(2,0))
sort_list(x)

## ------------------------------------------------------------------------
merge_lists <- function(x, y) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)
  
  i <- j <- k <- 1
  n <- length(x) + length(y)
  result <- vector("list", length = n)  
  
  while (i <= length(x) && j <= length(y)) {
    if (less(x[[i]], y[[j]])) {
      result[[k]] <- x[[i]]
      i <- i + 1
    } else {
      result[[k]] <- y[[j]]
      j <- j + 1
    }
    k <- k + 1
  }
  
  if (i > length(x)) {
    result[k:n] <- y[j:length(y)]
  } else {
    result[k:n] <- x[i:length(x)]
  }
  
  result
}

less <- function(x, y) UseMethod("less")
less.numeric <- function(x, y) x < y
less.tuple <- function(x, y) x[1] < y[1] || x[2] < y[2]

sort_list(x)

## ---- echo=FALSE---------------------------------------------------------
merge_lists <- function(x, y, less) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)
  
  i <- j <- k <- 1
  n <- length(x) + length(y)
  result <- vector("list", length = n)  
  
  while (i <= length(x) && j <= length(y)) {
    if (less(x[[i]], y[[j]])) {
      result[[k]] <- x[[i]]
      i <- i + 1
    } else {
      result[[k]] <- y[[j]]
      j <- j + 1
    }
    k <- k + 1
  }
  
  if (i > length(x)) {
    result[k:n] <- y[j:length(y)]
  } else {
    result[k:n] <- x[i:length(x)]
  }
  
  result
}

## ------------------------------------------------------------------------
sort_list <- function(x, less = `<`) {
  
  if (length(x) <= 1) return(x)
  
  result <- vector("list", length = length(x))
  
  start <- 1
  end <- length(x)
  middle <- end %/% 2
  
  merge_lists(sort_list(x[start:middle], less), 
              sort_list(x[(middle+1):end], less), 
              less)
}

unlist(sort_list(as.list(sample(1:5))))

tuple_less <- function(x, y) x[1] < y[1] || x[2] < y[2]
sort_list(x, tuple_less)

## ------------------------------------------------------------------------
foo <- function(object) UseMethod("foo")
foo.numeric <- function(object) object
foo(4)

bar <- function(object) {
  x <- 2
  UseMethod("bar")
}
bar.numeric <- function(object) x + object
bar(4)

## ------------------------------------------------------------------------
baz <- function(object) UseMethod("baz") + 2
baz.numeric <- function(object) object
baz(4)

