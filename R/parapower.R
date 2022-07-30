#' Parametric Power Analysis
#'
#' Parametric effect size related functions for tests along the lines of balanced ANOVAs
#' (e.g., certain t-tests, correlations, and regressions).
#'
#' @param effect Effect size of the specified type.
#' @param type Type of effect entered. Available types are Pearson's r (\code{r}), Cohen's d
#' (\code{d}), Area Under the Curve (AUC; \code{a}), Odds Ratio (\code{o}), η2 (eta-squared; R2;
#' \code{e}), or Cohen's f (\code{f}).
#' @param N Total sample size.
#' @param k Number of groups or cells, with 2 being the default and most generally appropriate.
#' @param adjust_f Logical indicating whether the entered effect of type \code{f} should be converted
#' from F to Cohen's f based on \code{N} and \code{k}: \code{sqrt((k - 1) * F / (N - k))}.
#' @param display_f Logical; if TRUE, calculates F and an associated p-value based on
#' \code{N} and \code{k}.
#' @param print Logical; if \code{FALSE}, results are only invisibly returned.
#' @param alpha Alpha level (p-value; theoretical type I error / false positive).
#' @param power Power (\code{1 – β}; inverse theoretical type II error / false negative).
#' @param range Interval of the estimated component for the optimizer to try. Defaults to
#' \code{c(2, 10000)} for \code{N}, \code{c(0, 100)} for \code{effect}, and
#' \code{c(0, 1)} for \code{power} or \code{alpha}. If you get an error from \code{uniroot} (like
#' \code{"f() values at end points not of opposite sign"}), try increasing the range
#' (e.g., smaller low end for large effects, and larger high end for small effects).
#' @details
#' This type of power analysis becomes increasingly difficult with more complex models.
#' In those cases, simulation is very handy (though more difficult to abstract in terms
#' of specification) –- if you can reasonably simulate the data you have in mind,
#' you can reasonably estimate power for any model. See the Monte Carlo section of the examples.
#' @examples
#' # a priori power analysis (looking for n given an effect size, alpha, and power)
#' # with no arguments, this defaults to effect = c(.3, .5, .7), type = 'd'
#' parapower()
#'
#' # sensitivity analysis (looking for an effect size given n, alpha, and power)
#' parapower(N = 50)
#'
#' # post-hoc power analysis (looking for power given an effect size, n, and alpha)
#' # since 'f' is the default type, this is power given Cohen's f = .3
#' parapower(.3, N = 50)
#'
#' # criterion analysis (looking for alpha given an effect size, n, and power)
#' parapower(.3, N = 50, power = .8)
#'
#' #
#' # Monte Carlo power analysis
#' #
#'
#' # simulate a simple effect: continuous normal x and related y
#' N <- 20
#' x <- rnorm(N)
#' y <- x * .4 + rnorm(N)
#' summary(lm(y ~ x))
#'
#' ## here, we can input the correlation to get other effect sizes
#' ## k defaults to 2, which is appropriate in this case
#' convert_effect(cor(x, y), N = N)
#'
#' ## we want to know what N gives us .8 power to detect this effect at .05 alpha
#' ## we can set up a function to easily try different Ns
#' sim1 <- function(N, beta = .4, batches = 100, alpha = .05, power = .8) {
#'   # first, set up a vector of 0s, which we'll fill with results from each run
#'   op <- numeric(batches)
#'   # now we repeat the simulation batches times
#'   # (with i tracking the index of op)
#'   for (i in seq_len(batches)) {
#'     # reroll the simulated variables
#'     x <- rnorm(N)
#'     y <- x * beta + rnorm(N)
#'     # set the current op entry to the p-value of the test
#'     op[i] <- summary(lm(y ~ x))$coef[2, 4]
#'   }
#'   # observed power is percent of p-values under alpha
#'   # output of the function is observed power - set power for optimization
#'   # (where the target is 0; closest observed to set power)
#'   mean(op < alpha) - power
#' }
#'
#' ## uniroot will try different values of N (between 20 and 100 here),
#' ## and output the one that gets closest to the set power
#' uniroot(sim1, c(20, 100))$root
#'
#' \dontrun{
#'
#' ## setting power to 0 will give the observed power
#' ## increase batches for more stable estimates
#' sim1(55, batches = 5000, power = 0)
#'
#' ## compare with a binary x
#' sim1b <- function(N, beta = .4, batches = 100, alpha = .05, power = .8) {
#'   op <- numeric(batches)
#'   for (i in seq_len(batches)) {
#'     x <- rep_len(c(0, 1), N)
#'     y <- x * beta + rnorm(N)
#'     op[i] <- summary(lm(y ~ x))$coef[2, 4]
#'   }
#'   mean(op < alpha) - power
#' }
#' uniroot(sim1b, c(100, 300))$root
#' sim1b(200, batches = 5000, power = 0)
#'
#' # this is what the most basic parametric power analysis is getting at
#' parapower(.4, "d")
#'
#' # simulate a more complicated effect: two binary variables with an interaction effect
#' c1 <- sample(c(0, 1), N, TRUE)
#' c2 <- sample(c(0, 1), N, TRUE)
#' y <- c2 * c1 * .8 + rnorm(N)
#' (m <- summary(lm(y ~ c1 * c2)))
#'
#' ## R-squared is eta^2, and k is 4 (2x2)
#' convert_effect(m$r.squared, "eta^2", N, 4)
#'
#' ## same kind of function as before, only difference is the simulated data and test
#' ## also have to recalculate the omnibus p-value since it's not returned
#' sim2 <- function(N, beta = .8, batches = 100, alpha = .05, power = .8) {
#'   op <- numeric(batches)
#'   for (i in seq_len(batches)) {
#'     c1 <- sample(c(0, 1), N, TRUE)
#'     c2 <- sample(c(0, 1), N, TRUE)
#'     y <- c2 * c1 * beta + rnorm(N)
#'     f <- summary(lm(y ~ c1 * c2))$fstatistic
#'     op[i] <- pf(f[1], f[2], f[3], lower.tail = FALSE)
#'   }
#'   mean(op < alpha) - power
#' }
#' uniroot(sim2, c(50, 300))$root
#' sim2(100, batches = 5000, power = 0)
#'
#' ## note here that we're looking at the omnibus test
#' ## if you're actually interested in, say, the interaction effect, that may affect power
#' sim2i <- function(N, beta = .8, batches = 100, alpha = .05, power = .8) {
#'   op <- numeric(batches)
#'   for (i in seq_len(batches)) {
#'     c1 <- sample(c(0, 1), N, TRUE)
#'     c2 <- sample(c(0, 1), N, TRUE)
#'     y <- c2 * c1 * beta + rnorm(N)
#'     op[i] <- summary(lm(y ~ c1 * c2))$coef[4, 4]
#'   }
#'   mean(op < alpha) - power
#' }
#' uniroot(sim2i, c(100, 300))$root
#' sim2i(200, batches = 5000, power = 0)
#' }
#' @export

parapower <- function(effect = NULL, type = "f", N = NULL, k = 2, alpha = .05, power = .8, range) {
  ck <- c(
    e = is.null(effect), N = is.null(N),
    power = missing(power) || is.null(power),
    alpha = missing(alpha) || is.null(alpha)
  )
  if (!any(ck)) {
    ck["e"] <- TRUE
    warning("estimating effect, since n, alpha, and power were all supplied")
  }
  if (ck["e"] && ck["N"]) {
    effect <- c(.3, .5, .7)
    type <- "d"
    ck["e"] <- FALSE
  } else if (ck["N"] && (!ck["power"] || !ck["alpha"])) {
    N <- 50
    ck["N"] <- FALSE
  }
  if (is.null(alpha) && is.null(power)) {
    alpha <- .05
    ck["alpha"] <- FALSE
    warning("setting alpha to .05, since power was set to NULL")
  } else if (is.null(alpha)) ck["power"] <- FALSE
  if (missing(range)) range <- if (ck["N"]) c(2, 1e4) else c(0, if (!ck["e"]) 1 else 100)
  par <- names(which(ck))[1]
  pars <- list(effect = effect, N = N, alpha = alpha, power = power)
  l <- vapply(pars, length, 0)
  ll <- if (all(l == max(l))) l[par] else which.max(l)
  l <- max(l)
  for (e in seq_along(pars)) {
    if (l > length(pars[[e]])) {
      pars[[e]] <- rep_len(if (is.null(pars[[e]])) NA else pars[[e]], l)
    }
  }
  op <- as.data.frame(matrix(0, l, 15, dimnames = list(
    paste(names(ll), "=", pars[[ll]]), c(
      "n", "k", "N", "alpha", "power", "Pearson's r", "Cohen's d", "AUC", "Odds Ratio",
      "eta^2", "Cohen's f", "F", "num_df", "den_df", "p-value"
    )
  )))
  fun <- function(i) {
    if (par == "N") N <- i else if (par == "e") f <- i else if (par == "power") p <- i else a <- i
    df2 <- (N - 1) * k
    pf(qf(1 - a, k - 1, df2), k - 1, df2, f^2 * N * k, lower.tail = FALSE) - p
  }
  for (set in seq_len(l)) {
    N <- pars$N[set]
    f <- if (!ck["e"]) convert_effect(pars$e[set], type, print = FALSE)[["Cohen's f"]] else NULL
    a <- pars$a[set]
    p <- pars$p[set]
    opt <- if (par == "power") fun(0) else uniroot(fun, range)$root
    if (par == "N") N <- opt else if (par == "e") f <- opt else if (par == "power") p <- opt else a <- opt
    op[set, ] <- c(0, k, N, a, p, convert_effect(f, "f", N, k, adjust_f = FALSE, print = FALSE))
    if (!ck["e"]) op[set, par] <- if (ck["N"]) round(opt + .499) else opt
  }
  op$n <- op$N
  op$N <- op$n * k
  print(op, digits = 4, row.names = FALSE)
  invisible(op)
}

#' @rdname parapower
#' @export

convert_effect <- function(effect, type = "r", N = 3, k = 2, adjust_f = TRUE, display_f = FALSE, print = TRUE) {
  type <- tolower(substr(sub("(cohen|pearson).* ", "", type, TRUE), 1, 1))
  df1 <- k - 1
  df2 <- N - k
  if (adjust_f && type == "f" && !missing(N)) effect <- sqrt(df1 * effect / df2)
  d <- switch(type,
    r = (2 * effect) / sqrt(1 - effect^2),
    d = effect,
    a = sqrt(2) * qnorm(effect),
    o = log(effect) * sqrt(3) / pi,
    e = sqrt(effect / (1 - effect)) * 2,
    f = effect * 2
  )
  f <- d / 2
  op <- as.data.frame(cbind(
    "Pearson's r" = d / sqrt((d^2) + 4),
    "Cohen's d" = d,
    "AUC" = pnorm(d / sqrt(2)),
    "Odds Ratio" = exp(d * pi / sqrt(3)),
    "eta^2" = f^2 / (1 + f^2),
    "Cohen's f" = f
  ))
  if (display_f || (!missing(N) && missing(display_f))) {
    op$`F` <- f^2 / df1 * df2
    op$`num_df` <- df1
    op$`den_df` <- df2
    op$`p-value` <- pf(abs(op$`F`), df1, df2, lower.tail = FALSE)
  }
  if (print) print(op, digits = 4, row.names = FALSE)
  invisible(op)
}

#' @rdname parapower
#' @export

critical_effect <- function(N, k = 2, alpha = .05) convert_effect(qf(1 - alpha, k - 1, N - k), "f", N, k)
