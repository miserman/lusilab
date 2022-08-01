#' Simple Clustering
#'
#' Clusters columns in a matrix based on a very minimal algorithm:
#' \itemize{
#'   \item Start with the column with the biggest sum / smallest other-cluster weight.
#'   \item Calculate its correlation with all other columns, and use that to define a cluster.
#'   \item Repeat with all unassigned columns.
#' }
#' \code{taffyInf} differs in that it will not eliminate columns from the pool, but will
#' base the selection of an initial term on the weight across all previously assigned clusters.
#'
#' @param m A numeric matrix with column names.
#' @param k Number of clusters to look for. This will be the maximum number for \code{taffy},
#' which will stop when the number of columns in the cluster is less than \code{minterm}.
#' For \code{taffyInf} this will always be the number returned, and columns may repeat between clusters.
#' @param minterm Minimum number of columns a cluster must have to be considered a cluster.
#' @param co Quantile-based cutt-off used to assign columns to a cluster.
#' @returns A list with vectors of column names (\code{taffy}), or a matrix of weights,
#' with a column for each cluster, and row for each column (\code{taffyInf}).
#' @examples
#' m <- Matrix(as.matrix(data.frame(
#'   cluster1_term1 = c(1, 1, 0, 0),
#'   cluster1_term2 = c(1, 0, 0, 0),
#'   cluster2_term2 = c(0, 0, 0, 1),
#'   cluster2_term1 = c(0, 0, 1, 1),
#'   cluster3_term1 = c(1, 0, 0, 1),
#'   cluster4_term1 = c(0, 1, 1, 0)
#' )))
#' taffy(m, co = .6)
#' taffyInf(m)
#' @export

taffy <- function(m, k = nrow(m), minterm = 2, co = .975) {
  w <- colSums(m)
  m <- t(m)
  ts <- list()
  su <- w != 0
  while (sum(su) && length(ts) < k) {
    l <- lma_simets(m[names(which.max(w[su])), ], m[su, ], metric = "pearson")
    f <- names(which(l > quantile(l, co, na.rm = TRUE)))
    if (length(f) < minterm) break
    ts <- c(ts, list(f))
    su[f] <- FALSE
  }
  ts
}

#' @rdname taffy
#' @export

taffyInf <- function(m, k = 2) {
  ks <- seq_len(k)
  w <- colSums(m)
  m <- t(m)
  l <- matrix(0, k, nrow(m), dimnames = list(ks, rownames(m)))
  for (i in ks) {
    h <- if (i == 1) which.max(w) else which.max(w * -colMeans(l))
    l[i, ] <- lma_simets(m[h, ], m, metric = "pearson")
  }
  t(l)
}
