#' Bootstrapped / Permuted Mediation
#'
#' Displays the results of Sobel and permutation/bootstrapping tests for simple mediation.
#'
#' Bootstrap confidence intervals indicate significance when they do not include zero (as they are
#' confidence intervals of the effect). Permutation confidence intervals indicate significance when
#' they do not include the indirect effect (as they are confidence intervals of the null effect).
#'
#' @param x X variable: can be the name of a variable in the environment or data.
#' @param y Y variable: can be the name of a variable in the environment or data.
#' @param m Mediating variable: can be the name of a variable in the environment or data.
#' @param data \code{data.frame} from which to pull variable, with the global environment as a fallback.
#' @param cov A character vector of covariates to include in each path.
#' Only accepted if \code{data} is also specified.
#' @param random Variable to be included as a random intercept in an \code{\link[nlme]{lme}} model:
#' \code{lme(y ~ x + m, random = ~ 1 | random)}
#' @param su Subset applied across variables, e.g., \code{su = x < 2 & m > 0}.
#' @param i The number of iterations to be processed for bootstrapping. Default is \code{1000}.
#' @param method Character specifying the test type. Anything starting with \code{b} will perform
#' bootstrapping, otherwise will performs the non-iterative, permutation-of-residuals approach as
#' described in
#' \href{https://link.springer.com/article/10.3758/s13428-011-0181-x}{Taylor and MacKinnon (2012; Eqs. 9 and 10)}.
#' @param alpha The alpha level for the bootstrapping confidence interval.
#' Default is \code{.05} (i.e., 2.5% and 97.5%).
#' @param figure Logical; if \code{FALSE}, will not display a figure.
#' @param ... Passes additional arguments to \code{\link{medfig}} if \code{figure} is \code{TRUE}.
#' @seealso \code{\link{medfig}} to make a figure from results.
#' @references
#' Taylor, A. B., & MacKinnon, D. P. (2012). Four applications of permutation methods to testing a
#' single-mediator model. \emph{Behavior research methods, 44}, 806-844. doi:
#' \href{https://doi.org/10.3758/s13428-011-0181-x}{10.3758/s13428-011-0181-x}
#' @examples
#' # This displays the mediation of weight on the relationship
#' # between displacement and miles per gallon.
#' bspmed(disp, mpg, wt, scale(mtcars))
#' @export

bspmed <- function(x, y, m, data = NULL, cov = NULL, random = NULL, su = NA, i = 1000,
                   method = "permutation", alpha = .05, figure = TRUE, ...) {
  txt <- list(
    x = gsub('^\\\\*"|\\\\*"$', "", deparse(substitute(x, environment()))),
    y = gsub('^\\\\*"|\\\\*"$', "", deparse(substitute(y, environment()))),
    m = gsub('^\\\\*"|\\\\*"$', "", deparse(substitute(m, environment()))),
    random = gsub('^\\\\*"|\\\\*"$', "", deparse(substitute(random, environment())))
  )
  rand <- !missing(random)
  covar <- if (is.null(data) && !missing(cov)) {
    warning("data must be specified to include covariates", call. = FALSE)
    FALSE
  } else {
    !missing(cov)
  }
  boot <- grepl("^b", method, TRUE)
  if (!is.null(data)) data <- as.data.frame(data)
  tdc <- function(a, data = NULL) {
    ta <- a
    if (is.character(ta) && (length(ta) == 1 || !any(grepl(" ", ta, fixed = TRUE)))) ta <- parse(text = a)
    ta <- tryCatch(eval(ta, data, parent.frame(2)), error = function(e) NULL)
    if (is.null(ta)) ta <- tryCatch(eval(ta, data), error = function(e) NULL)
    if (is.null(ta)) stop("could not find ", a, call. = FALSE)
    ta
  }
  dat <- data.frame(
    x = tdc(txt$x, data),
    y = tdc(txt$y, data),
    m = tdc(txt$m, data)
  )
  names(dat) <- with(txt, c(x, y, m))
  if (rand) dat$random <- tdc(txt$random, data)
  if (covar) dat <- cbind(dat, data[, cov])
  for (c in seq_len(ncol(dat))) if (is.factor(dat[, c])) dat[, c] <- as.numeric(dat[, c])
  mot <- paste0("Effect of ", txt$x, " on ", txt$y, ", by way of ", txt$m)
  cat(mot, "\n")
  if (rand) cat("Random:", paste0("~1|", txt$random), "\n")
  if (covar) cat("Covariates:", paste0(cov, collapse = " + "), "\n")
  if (!missing(su)) {
    cat("Subset:", deparse(substitute(su, environment())), "\n")
    dat <- if (!missing(data)) dat[eval(substitute(su, environment()), data), ] else dat[su, ]
  }
  dat <- na.omit(dat)
  t <- proc.time()[3]
  ss <- seq_len(nrow(dat))
  amod <- as.formula(with(txt, paste(m, "~", x, if (covar) paste(" +", paste(cov, collapse = " + ")))))
  cmod <- as.formula(with(txt, paste(y, "~", x, if (covar) paste(" +", paste(cov, collapse = " + ")))))
  bmod <- as.formula(with(txt, paste(y, "~", m, "+", x, if (covar) paste(" +", paste(cov, collapse = " + ")))))
  if (!rand) {
    a <- lm(amod, dat)
    b <- lm(bmod, dat)
    ab <- if (boot) {
      vapply(seq_len(i), function(p) {
        nd <- dat[sample(ss, replace = TRUE), ]
        lm(amod, nd)$coef[2] * lm(bmod, nd)$coef[3]
      }, 0)
    } else {
      pr <- lapply(list(a, b), function(m) list(f = fitted(m), r = resid(m)))
      vapply(seq_len(i), function(p) {
        dat[, txt$m] <- pr[[1]]$f + sample(pr[[1]]$r)
        dat[, txt$y] <- pr[[2]]$f + sample(pr[[2]]$r)
        lm(amod, dat)$coef[2] * lm(bmod, dat)$coef[3]
      }, 0)
    }
    a <- summary(a)$coef
    b <- summary(b)$coef
  } else {
    gs <- unique(dat$random)
    a <- lme(amod, dat, ~ 1 | random, na.action = na.omit)
    b <- lme(bmod, dat, ~ 1 | random, na.action = na.omit)
    ab <- if (boot) {
      vapply(seq_len(i), function(p) {
        nd <- dat[dat$random %in% sample(gs, replace = T), ]
        a <- tryCatch(summary(lme(amod, nd, ~ 1 | random, na.action = na.omit))$tTable[2, 1], error = function(e) NULL)
        b <- tryCatch(summary(lme(bmod, nd, ~ 1 | random, na.action = na.omit))$tTable[3, 1], error = function(e) NULL)
        if (!is.null(a) && !is.null(b)) a * b else NA
      }, 0)
    } else {
      pr <- lapply(list(a, b), function(m) list(f = fitted(m), r = resid(m)))
      vapply(seq_len(i), function(p) {
        dat[, txt$m] <- pr[[1]]$f + sample(pr[[1]]$r)
        dat[, txt$y] <- pr[[2]]$f + sample(pr[[2]]$r)
        a <- tryCatch(summary(lme(amod, dat, ~ 1 | random, na.action = na.omit))$tTable[2, 1], error = function(e) NULL)
        b <- tryCatch(summary(lme(bmod, dat, ~ 1 | random, na.action = na.omit))$tTable[3, 1], error = function(e) NULL)
        if (!is.null(a) && !is.null(b)) a * b else NA
      }, 0)
    }
    a <- summary(a)$tTable[, c(1, 2, 4, 5)]
    b <- summary(b)$tTable[, c(1, 2, 4, 5)]
    if ((fr <- mean(is.na(ab))) > .7) stop(round(fr * 100, 2), "% of the lme models failed")
  }
  cat("Processed in", proc.time()[3] - t, "seconds\n\n")
  message("Sobel")
  m <- matrix(0, 2, 5, dimnames = list(c("b", "p"), c("a", "b", "c", "c'", "indirect")))
  c <- if (!rand) summary(lm(cmod, data = dat))$coef else summary(lme(cmod, data = dat, random = ~ 1 | random))$tTable[, c(1, 2, 4, 5)]
  ie <- a[2, 1] * b[2, 1]
  m[1, ] <- c(a[2, 1], b[2, 1], c[2, 1], b[3, 1], ie)
  m[2, ] <- c(a[2, 4], b[2, 4], c[2, 4], b[3, 4], (1 - pnorm(abs(ie / sqrt(b[2, 1]^2 * a[2, 2]^2 + a[2]^2 * b[2, 2]^2)))) * 2)
  print(round(m, 4))
  p <- qnorm(sum(na.omit(ab) <= ie) / i)
  cat("\n")
  message(if (boot) "Bootstrap confidence intervals" else "Permutation confidence intervals")
  ci <- quantile(ab, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  if (boot) {
    res <- cbind(res <- as.data.frame(matrix(c(ci, quantile(ab, c(pnorm(2 * p + qnorm(alpha / 2)), pnorm(2 * p + qnorm(1 - alpha / 2))),
      na.rm = TRUE
    )), ncol = 2, dimnames = list(c("Uncorrected", "Corrected"), c(
      paste0((alpha / 2) * 100, "%"),
      paste0((1 - alpha / 2) * 100, "%")
    )), byrow = T)), `all <|> 0` = apply(res, 1, function(r) all(r < 0) || all(r > 0)))
  } else {
    res <- cbind(as.data.frame(matrix(ci, 1)), all(ci < ie) || all(ci > ie))
    rownames(res) <- "CI"
    colnames(res) <- c(names(ci), paste("all <|>", round(ie, 4)))
  }
  print(res)
  res <- list(effects = ab, vars = txt, sobel = m, ci.type = if (boot) "Bootstrap" else "Permutation", ci = res)
  if (figure) medfig(res, ...)
  invisible(res)
}

#' Mediation Figure
#'
#' Plot simple mediation figures, maybe displaying indirect effects and confidence intervals of
#' some sort, and potentially saving them.
#'
#' @param mat Output from \code{\link{bspmed}}, or a matrix with two rows and 4-5 columns (see example).
#' @param ci A vector of confidence intervals (one lower and one upper). If the entries are named,
#' these will be read as numbers and added together for the displayed percent (see example).
#' @param ci.type Text to be displayed in the note (i.e., \code{ci.type} confidence intervals).
#' @param xlab,ylab,mlab Text to be displayed in each variable box.
#' @param title,note,box Logical indicating whether to draw each element.
#' @param digits Number of digits to round to.
#' @param note.sig Logical indicating whether asterisks corresponding to p-values should be added
#' to numbers.
#' @param fin.length Length of the arrowhead fins, back from the line intersection (0 would be a
#' flat base, for a triangular arrowhead).
#' @param cords.box Center position of each box. Should be a list with at least one coordinate pair
#' (a vector with values for y and x position). If entries aren't named, these x, y, and m are
#' assigned as names; coordinates pairs can be entered positionally or named.
#' Default is \code{list(x = c(.1, .1), m = c(.5, .9), y = c(.9, .1))}.
#' @param cords.arrow Position of each arrow. This behaves in the same way as cords.box, but
#' entries are names a, b, and c, and each set of coordinates should be 4 values specifying
#' start and end positions. Any unspecified coordinates are calculated based on box positions.
#' @param padding.box,padding.arrow,padding.number,padding.title,padding.note Specifies the padding
#' around each of the elements; box adjust space around texts in each box, arrow adjust distance
#' from boxes, and number adjusts space between numbers and lines.
#' @param arrow.scale Size of each arrowhead.
#' @param box.fill Color of the box backgrounds.
#' @param center.adj Amount to shift the center text downward (positive values making center text closer to the bottom).
#' @param color A vector with named entries corresponding to any of the elements
#' (line, box, label, number, title, note). Box refers to the color of the line
#' (whereas \code{box.fill} specifies the background). Single unnamed values are applied to all elements.
#' @param weight The weight of arrow lines or box outlines, or size of numbers or texts. Behaves the same as color.
#' @param save,format,name Information used when saving the figure to a file; if any are specified,
#' an image is saved. Format refers to the graphics engine; default is \code{pdf}. Some elements may not
#' show up properly using other engines (such as \code{cairo_pdf}). Other engines are included in the base
#' \code{link[grDevices]{grDevices}} package. Another option is \code{\link[devEMF]{emf}} from the
#' \code{devEMF} package (used to create the example image).
#' @param ... Additional arguments passed to par (parameters of the plot frame).
#' @seealso \code{\link{bspmed}} to run a mediation model.
#' @examples
#' # by default, the bspmed function will show a figure
#' bspmed(disp, mpg, wt, scale(mtcars))
#'
#' # you can also save the output from a bspmed call and plot a figure for it later:
#' med <- bspmed(disp, mpg, wt, scale(mtcars), figure = FALSE)
#' medfig(med)
#'
#' # Or you can enter a matrix, with confidence intervals in the second position,
#' # and what type they are in the third.
#' mat <- matrix(
#'   c(.89, .0001, -.54, .001, -.85, .0001, -.36, .05, -.48, .001),
#'   2,
#'   dimnames = list(c("b", "p"), c("a", "b", "c", "c'", "i"))
#' )
#' medfig(mat, c("2.5" = -.38, "97.5" = .37), "Permutation")
#' @export

medfig <- function(mat, ci = NULL, ci.type = NULL, xlab = "x variable", ylab = "y variable", mlab = "mediating variable",
                   title = FALSE, note = TRUE, box = TRUE, digits = 3, note.sig = TRUE, fin.length = .5, cords.box = list(), cords.arrow = list(),
                   padding.box = .05, padding.arrow = .02, padding.number = .02, padding.title = .1, padding.note = .1, arrow.scale = .01,
                   box.fill = NA, center.adj = .1, color = c(line = NA, box = NA, label = NA, number = NA, title = NA, note = NA),
                   weight = c(line = NA, box = NA, label = NA, number = NA, title = 2, note = .9), save = FALSE, format = pdf, name = "mediation", ...) {
  if (missing(mat)) stop("mat must be specified")
  if (is.list(mat) && "sobel" %in% names(mat)) {
    nm <- names(mat)
    if ("ci" %in% nm && missing(ci)) ci <- mat$ci
    if ("ci.type" %in% nm && missing(ci.type)) ci.type <- mat$ci.type
    if ("vars" %in% nm) {
      nm <- names(mat$vars)
      if ("x" %in% nm && missing(xlab)) xlab <- mat$vars$x
      if ("y" %in% nm && missing(ylab)) ylab <- mat$vars$y
      if ("y" %in% nm && missing(mlab)) mlab <- mat$vars$m
    }
    mat <- mat$sobel
  }
  if (!is.null(ci) && !is.null(nrow(ci))) ci <- ci[nrow(ci), , drop = FALSE]
  if (NCOL(mat) < 3) stop("mat must have at least 3 values")
  pn <- c("line", "box", "label", "number", "title", "note")
  dop <- par(no.readonly = TRUE)
  ea <- list(...)
  do.call(par, c(list(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0)), if (any(ons <- names(ea) %in% names(dop))) ea[ons]))
  on.exit(par(dop))
  if (!missing(color)) {
    color[is.null(color) | color == "none"] <- "NA"
    if (is.null(names(color))) if (length(color) == 1) color[pn] <- color else names(color) <- pn[seq_along(color)]
    color <- color[pn]
  }
  if (any(is.na(color))) {
    color <- color[!is.na(color)]
    color[pn[!pn %in% names(color)]] <- par("col")
  }
  if (!missing(weight)) {
    if (is.null(names(weight))) if (length(weight) == 1) weight[pn] <- weight else names(weight) <- pn[seq_along(weight)]
    weight <- weight[pn]
  }
  if (any(is.na(weight))) {
    weight <- weight[!is.na(weight)]
    mas <- pn[!pn %in% names(weight)]
    if (any(tmas <- mas %in% pn[1:2])) weight[mas[tmas]] <- par("lwd")
    if (any(tmas <- mas %in% pn[3:6])) weight[mas[tmas]] <- par("cex")
  }
  tn <- c(
    title = !missing(title) && (!is.logical(title) || title),
    note = missing(note) || (!is.logical(note) || note)
  )
  lo <- matrix(c(2, padding.title, 1, 1, 3, padding.note), 2)[, c(tn[["title"]], TRUE, tn[["note"]]), drop = FALSE]
  if (!tn[["title"]] && tn[["note"]]) lo[1, 2] <- 2
  layout(lo[1, ], heights = lo[2, ])
  plot.new()
  colnames(mat) <- c("a", "b", "c", "c'", "i")[seq_len(ncol(mat))]
  cn <- colnames(mat)
  txt <- list(x = xlab, m = mlab, y = ylab)
  sl <- vapply(txt, strwidth, 0) / max(1, 3 - weight[["label"]]) + padding.box / 2 * weight[["label"]]
  sp <- list(x = c(.1, .1), m = c(.5, .9), y = c(.9, .1))
  if (!missing(cords.box)) {
    if (is.null(names(cords.box))) names(cords.box) <- c("x", "m", "y")[seq_along(cords.box)]
    sp <- c(cords.box, sp[!names(sp) %in% names(cords.box)])
  }
  bx <- list()
  for (n in c("x", "y", "m")) {
    bx[[n]] <- list(
      max(0, sp[[n]][1] - sl[n]), max(0, sp[[n]][2] - padding.box),
      min(1, sp[[n]][1] + sl[n]), min(1, sp[[n]][2] + padding.box)
    )
    if (box) do.call(graphics::rect, c(bx[[n]], col = box.fill, border = color[["box"]], lwd = weight[["box"]]))
    do.call(graphics::text, c(as.list(sp[[n]]), txt[[n]], col = color[["label"]], cex = weight[["label"]]))
  }
  aco <- list(
    a = list(sp$x[1] + padding.arrow / 1.5, bx$x[[4]] + padding.arrow * 1.5, bx$m[[1]] - padding.arrow / 1.5, bx$m[[2]] - padding.arrow * 1.5),
    b = list(bx$m[[3]] + padding.arrow / 2, bx$m[[2]] - padding.arrow, sp$y[1] - padding.arrow / 1.5, bx$y[[4]] + padding.arrow * 1.5),
    c = list(bx$x[[3]] + padding.arrow / 1.5, sp$x[2], bx$y[[1]] - padding.arrow, sp$y[2])
  )
  if (!missing(cords.arrow) && is.list(cords.arrow) && any(names(cords.arrow) %in% names(aco))) {
    for (n in names(cords.arrow)) {
      if (length(cords.arrow[[n]]) == 4 && n %in% names(aco)) {
        aco[[n]] <- cords.arrow[[n]]
      }
    }
  }
  d <- c(((u <- par("usr"))[2] - u[1]) / (p <- (p <- par("pin")) / max(p))[1], (u[4] - u[3]) / p[2])
  s <- function(x0, y0, x1, y1) atan((y1 - y0) / (x1 - x0) * d[1] / d[2]) / pi * 180
  la <- weight[["line"]] / 2 / 100
  adj <- padding.number + la / 2
  adj <- list(a = c(-adj, adj), b = c(adj, adj), c = c(0, -adj - la * 5))
  mat <- vapply(as.data.frame(mat), function(p) {
    c(
      round(p[1], digits), p[2],
      c("***", "**", "*", "\u2020", "italic(ns)")[which(p[2] < c(.001, .01, .05, .1, Inf))[1]]
    )
  }, character(3))
  op <- function(sl) {
    s <- strsplit(sl, "")[[1]][1] == c("*", "\u2020", "i")
    paste0(c('*"', '^"', "^")[s], sl, c('"', '"', "")[s])
  }
  ct <- function(m, n) {
    if (note.sig) {
      paste0("expression(", m[1, n], op(as.character(m[3, n])), if (n == "c") {
        paste0("~(", m[1, "c'"], op(as.character(m[3, "c'"])), ")")
      }, ")")
    } else {
      paste0("expression(", m[1, n], if (n == "c") paste0("~(", m[1, "c'"], ")"), ")")
    }
  }
  ar <- function(l, a) {
    do.call(graphics::segments, c(l, col = color[["line"]], lwd = weight[["line"]]))
    l <- unlist(l)
    p <- matrix(c(
      l[3], l[4], l[3] - arrow.scale * fin.length, l[4] + arrow.scale, l[3] + arrow.scale,
      l[4], l[3] - arrow.scale * fin.length, l[4] - arrow.scale
    ), 2)
    cen <- matrix(rep(c(l[3], l[4]), 4), 2)
    a <- a / 180 * pi
    p <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2) %*% (p - cen) * d + cen
    if (l[1] > l[3]) p[, 2:4] <- p[, 2:4] + (p[, 1] - p[, 2:4]) * 2
    polygon(p[1, ], p[2, ], col = color[["line"]], lwd = weight[["line"]])
  }
  for (n in names(aco)) {
    co <- aco[[n]]
    an <- do.call(s, co)
    do.call(ar, list(co, an))
    text(mean(c(co[[1]], co[[3]])) + adj[[n]][1], mean(c(co[[2]], co[[4]])) + adj[[n]][2],
      eval(parse(text = ct(mat, n))),
      srt = an, col = color[["number"]], cex = weight[["number"]]
    )
  }
  if ("i" %in% cn) {
    it <- ct(mat, "i")
    if (!is.null(ci)) {
      l <- NA
      if (!is.null(names(ci))) l <- as.numeric(gsub("[^0-9.]", "", names(ci)[1]))
      l <- if (is.na(l)) "CI" else paste0((if (l > 1) 100 else 1) - l * 2, "% CI")
      ci[1:2] <- round(ci[1:2], digits)
      it <- sub(")$", paste0('*", ', l, " [", ci[[1]], ", ", ci[[2]], "]", '")'), it)
    }
    text(mean(c(mean(c(bx$x[[3]], bx$y[[1]])), sp$m[1])),
      mean(c(mean(c(bx$x[[4]], bx$y[[4]])), bx$m[[2]])) - center.adj, eval(parse(text = it)),
      col = color[["number"]], cex = weight[["number"]]
    )
  }
  if (tn[["title"]]) {
    plot.new()
    text(.5, .5, if (is.character(title)) title else paste("Effect of", xlab, "on", ylab, "by way of", ylab),
      col = color[["title"]], cex = weight[["title"]]
    )
  }
  if (tn[["note"]]) {
    plot.new()
    text(.5, .5, if (is.character(note)) {
      note
    } else {
      eval(parse(text = paste0("expression(paste(", paste(
        if (!is.null(ci.type)) paste('"', ci.type, 'confidence intervals; ",'),
        "p < .1^'\u2020',', ', p < .05*'*',', ', p < .01*'**',', ', p < .001*'***'", "))"
      ))))
    },
    col = color[["note"]], cex = weight[["note"]]
    )
  }
  if (save || !missing(format) || !missing(name)) {
    tryCatch(
      {
        t <- as.character(substitute(format, environment()))
        if (length(t) != 1) t <- t[length(t)]
        tt <- if (grepl("cairo", t, TRUE)) {
          paste0(".", if (grepl("_", t, fixed = TRUE)) strsplit(t, "_")[[1]][2] else sub("Cairo", "", t, fixed = TRUE))
        } else if (t == "postscript") ".ps" else paste0(".", t)
        dims <- if (grepl("jpeg|png|tiff|bmp|bit", t)) dev.size(units = "px") else dev.size()
        fn <- paste0(name, tolower(tt))
        dev.copy(format, fn, width = dims[1], height = dims[2])
        dev.off()
        message("image saved: ", getwd(), "/", fn)
      },
      error = function(e) warning("unable to save image: ", e$message, call. = FALSE)
    )
  }
  invisible(list(box.width = sl, box.center = sp, box.sides = bx, lines = aco))
}
