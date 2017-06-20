## ------------------------------------------------------------------------
weight_distribution <- function(mu, S) {
  structure(list(mu = mu, S = S), class = "wdist")
}
  
prior_distribution <- function(a) {
  mu = c(0, 0)
  S = diag(1/a, nrow = 2, ncol = 2)
  weight_distribution(mu, S)
}

## ------------------------------------------------------------------------
sample_weights <- function(n, distribution) {
  MASS::mvrnorm(n = n, 
                mu = distribution$mu,
                Sigma = distribution$S)
}


## ---- sampling_prior, fig.cap="Samples from the prior of lines."---------
prior <- prior_distribution(1)
(w <- sample_weights(5, prior))

plot(c(-1, 1), c(-1, 1), type = 'n',
     xlab = '', ylab = '')
plot_lines <- function(w) {
  for (i in 1:nrow(w)) {
    abline(a = w[i, 1], b = w[i, 2])
  }
}
plot_lines(w)

## ------------------------------------------------------------------------
fit_posterior <- function(x, y, b, prior) {
  mu0 <- prior$mu
  S0 <- prior$S
  
  X <- matrix(c(rep(1, length(x)), x), ncol = 2)
  
  S <- solve(S0 + b * t(X) %*% X)
  mu <- S %*% (solve(S0) %*% mu0 + b * t(X) %*% y)
  
  weight_distribution(mu = mu, S = S)
}

## ---- sampling_posterior, fig.cap = "Samples of posterior lines."--------
x <- rnorm(20)
y <- 0.2 + 1.3 * x + rnorm(20)
plot(x, y)

posterior <- fit_posterior(x, y, 1, prior)
w <- sample_weights(5, posterior)
plot_lines(w)

## ---- echo=FALSE---------------------------------------------------------
rm(x) ; rm(y)

## ------------------------------------------------------------------------
x <- rnorm(5)
y <- 1.2 + 2 * x + rnorm(5)
model.matrix(y ~ x)

## ------------------------------------------------------------------------
d <- data.frame(x, y)

## ------------------------------------------------------------------------
model.matrix(y ~ x, data = d)

## ------------------------------------------------------------------------
model.matrix(y ~ x - 1, data = d)

## ------------------------------------------------------------------------
model.matrix(y ~ x + I(x**2), data = d)

## ------------------------------------------------------------------------
model.frame(y ~ x + I(x**2), data = d)

## ------------------------------------------------------------------------
model.response(model.frame(y ~ x + I(x**2), data = d))

## ------------------------------------------------------------------------
prior_distribution <- function(formula, a, data) {
  n <- ncol(model.matrix(formula, data = data))
  mu <- rep(0, n)
  S <- diag(1/a, nrow = n, ncol = n)
  weight_distribution(mu, S)
}

## ------------------------------------------------------------------------
fit_posterior <- function(formula, b, prior, data) {
  mu0 <- prior$mu
  S0 <- prior$S
  
  X <- model.matrix(formula, data = data)
  
  S <- solve(S0 + b * t(X) %*% X)
  mu <- S %*% (solve(S0) %*% mu0 + b * t(X) %*% y)
  
  weight_distribution(mu = mu, S = S)
}

## ------------------------------------------------------------------------
d <- {
  x <- rnorm(5)
  y <- 1.2 + 2 * x + rnorm(5)
  data.frame(x = x, y = y)
}

prior <- prior_distribution(y ~ x, 1, d)
posterior <- fit_posterior(y ~ x, 1, prior, d)
posterior

## ------------------------------------------------------------------------
prior <- prior_distribution(y ~ x + I(x**2), 1, d)
posterior <- fit_posterior(y ~ x + I(x**2), 1, prior, d)
posterior

## ------------------------------------------------------------------------
blm <- function(formula, b, data, prior = NULL, a = NULL) {
  
  if (is.null(prior)) {
    if (is.null(a)) stop("Without a prior you must provide a.")
    prior <- prior_distribution(formula, a, data)
    
  } else {
    if (inherits(prior, "blm")) {
      prior <- prior$prior
    }
  }
  if (!inherits(prior, "wdist")) {
    stop("The provided prior does not have the expected type.")
  }
  
  posterior <- fit_posterior(formula, b, prior, data)
  
  structure(
    list(formula = formula,
         data = model.frame(formula, data),
         dist = posterior,
         call = match.call()),
    class = "blm"
  )
}

## ------------------------------------------------------------------------
print.blm <- function(x, ...) {
  print(x$call)
}

## ------------------------------------------------------------------------
(model <- blm(y ~ x + I(x**2), a = 1, b = 1, data = d))

## ------------------------------------------------------------------------
coef.blm <- function(object, ...) {
  t(object$dist$mu)
}
coef(model)

## ------------------------------------------------------------------------
confint.blm <- function(object, parm, level = 0.95, ...) {
  if (missing(parm)) {
    parm <- rownames(object$dist$mu)
  }
  
  means <- object$dist$mu[parm,]
  sds <- sqrt(diag(object$dist$S)[parm])
  
  lower_q <- qnorm(p = (1-level)/2, 
                   mean = means, 
                   sd = sds)
  upper_q <- qnorm(p = 1 - (1-level)/2, 
                   mean = means, 
                   sd = sds)
  
  quantiles <- cbind(lower_q, upper_q)
  quantile_names <- paste(
    100 * c((1-level)/2, 1 - (1 -level)/2),
    "%",
    sep = ""
    )
  colnames(quantiles) <- quantile_names
  
  quantiles
}

confint(model)

## ------------------------------------------------------------------------
rm(x) ; rm(y)

## ------------------------------------------------------------------------
model.matrix(y ~ x + I(x**2), data = d)

## ------------------------------------------------------------------------
dd <- data.frame(x = rnorm(5))
model.matrix(y ~ x + I(x**2), data = dd)

## ------------------------------------------------------------------------
model.matrix(delete.response(terms(y ~ x)), data = dd)

## ------------------------------------------------------------------------
predict.blm <- function(object, newdata, ...) {
  updated_terms <- delete.response(terms(object$formula))
  X <- model.matrix(updated_terms, data = newdata)
  
  predictions <- vector("numeric", length = nrow(X))
  for (i in seq_along(predictions)) {
    predictions[i] <- t(object$dist$mu) %*% X[i,]
  }
  predictions
}

predict(model, d)

## ----true-versus-predicted, fig.cap = "True versus predicted values"-----
d <- {
  x <- rnorm(50)
  y <- 0.2 + 1.4 * x + rnorm(50)
  data.frame(x = x, y = y)
}
model <- blm(y ~ x, d, a = 1, b = 1)
plot(d$y, predict(model, d),
     xlab = "True responses",
     ylab = "Predicted responses")

## ------------------------------------------------------------------------
fitted.blm <- function(object, ...) {
  predict(object, newdata = object$data, ...)
}

## ---- echo=FALSE---------------------------------------------------------
blm <- function(formula, b, data, prior = NULL, a = NULL) {
  
  if (is.null(prior)) {
    if (is.null(a)) stop("Without a prior you must provide a.")
    prior <- prior_distribution(formula, a, data)
    
  } else {
    if (inherits(prior, "blm")) {
      prior <- prior$prior
    }
  }
  if (!inherits(prior, "wdist")) {
    stop("The provided prior does not have the expected type.")
  }
  
  posterior <- fit_posterior(formula, b, prior, data)
  
  structure(
    list(formula = formula,
         data = model.frame(formula, data),
         dist = posterior,
         precision = b,
         call = match.call()),
    class = "blm"
  )
}

## ------------------------------------------------------------------------
predict.blm <- function(object, newdata, 
                        intervals = FALSE,
                        level = 0.95,
                        ...) {
  
  updated_terms <- delete.response(terms(object$formula))
  X <- model.matrix(updated_terms, data = newdata)
  
  predictions <- vector("numeric", length = nrow(X))
  for (i in seq_along(predictions)) {
    predictions[i] <- t(object$dist$mu) %*% X[i,]
  }
  
  if (!intervals) return(predictions)
  
  S <- model$dist$S
  b <- model$precision
  sds <- vector("numeric", length = nrow(X))
  for (i in seq_along(predictions)) {
    sds[i] <- sqrt(1/b + t(X[i,]) %*% S %*% X[i,])
  }

  lower_q <- qnorm(p = (1-level)/2, 
                   mean = predictions, 
                   sd = sds)
  upper_q <- qnorm(p = 1 - (1-level)/2, 
                   mean = predictions, 
                   sd = sds)
  
  intervals <- cbind(lower_q, predictions, upper_q)
  colnames(intervals) <- c("lower", "mean", "upper")
  as.data.frame(intervals)
}

model <- blm(y ~ x, d, a = 1, b = 1)

## ----predictions-with-intervals, fig.cap = "Predictions versus true values with confidence intervals."----
require(ggplot2)
predictions <- fitted(model, intervals = TRUE)
ggplot(cbind(data.frame(y = d$y), predictions),
       aes(x = y, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_abline(slope = 1) + 
  xlab("True responses") +
  ylab("Predictions") +
  theme_minimal()

