#' Get Baby Names Data
#'
#' Downloads and processes historical baby names data from the U.S.
#' \href{https://www.ssa.gov/OACT/babynames/limits.html}{Social Security Administraction}.
#' @param dir Directory in which to save data.
#' @param source Which dataset to download; between \code{national} (default), \code{state}, and \code{territory}.
#' @returns A list with entries for \code{data} (a \code{data.frame} with the original data), and
#' \code{summary} (a \code{matrix} with a count of each name and probabilities per sex).
#' @examples
#' \dontrun{
#' # saves to a temporary directory
#' names <- get_baby_names()
#' }
#' @export

get_baby_names <- function(dir = tempdir(), source = "national") {
  type <- tolower(substring(source, 1, 1))
  s <- c(n = "names.zip", s = "state/namesbystate.zip", t = "territory/namesbyterritory.zip")[type]
  if (is.na(s)) stop("source not recognized", call. = FALSE)
  out <- normalizePath(paste0(dir, "/", basename(s)), "/", FALSE)
  final <- sub(".zip", "", out, fixed = TRUE)
  if (!dir.exists(final)) {
    if (!file.exists(out)) download.file(paste0("https://www.ssa.gov/OACT/babynames/", s), out, quite = TRUE)
    unzip(out, exdir = final)
    unlink(out)
  }
  if (!dir.exists(final)) stop("download failed", call. = FALSE)
  files <- list.files(final, "txt", ignore.case = TRUE, full.names = TRUE)
  data <- do.call(rbind, lapply(files, function(f) {
    d <- read.csv(f, header = FALSE)
    if (type == "n") d$year <- as.numeric(regmatches(f, regexec("\\d{4}", f))[[1]])
    d
  }))
  colnames(data) <- list(
    n = c("name", "sex", "count", "year"),
    s = c("state", "sex", "year", "name", "count"),
    t = c("territory", "sex", "year", "name", "count")
  )[[type]]
  summary <- t(vapply(split(data, data$name), function(d) {
    fem <- sum(d[d$sex == "F", "count"]) / sum(d$count)
    c(count = nrow(d), female = fem, male = 1 - fem)
  }, numeric(3)))
  list(data = data, summary = summary)
}
