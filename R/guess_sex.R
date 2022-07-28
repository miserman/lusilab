#' Guess sex based on a web search
#'
#' Searches for a name (or whatever), and looks for gendered pronouns or other basic cues to sex.
#'
#' @param names A character vector of search terms (such as a name).
#' @param retry Logical; if \code{TRUE}, will retry after a failed attempt.
#' @param search_source URL of the search engine; each entry in \code{names}
#' is simply appended.
#' @param cores Number of CPU cores to split \code{names} across.
#' @returns a \code{data.frame} with entries for \code{name} (the original search term),
#' \code{guess}, \code{confidence}, and \code{female} and \code{male} with count of matches.
#' @examples
#' \dontrun{
#' guess_sex(c("Jane", "John"))
#' }
#' @export
#'

guess_sex <- function(names, retry = 50, search_source = "https://search.yahoo.com/search?q=", cores = detectCores() - 2) {
  search <- function(term, source = search_source,
                     female = " she | she's | her | hers | woman | (wife|sister|mother|gandmother|aunt) (of|to) ",
                     male = " he | he's | him | his | man | (husband|brother|father|grandfather|uncle) (of|to) ",
                     attempt = if (is.numeric(retry)) retry else 0) {
    req <- httr::GET(paste0(source, gsub("[ _&-]+", "+", term)))
    if (req$status_code == 200) {
      p <- paste("", gsub("\\W+", " ", tolower(paste0(httr::content(req, "text"), collapse = " "))), "")
      fem <- length(strsplit(p, female, perl = TRUE)[[1]])
      mal <- length(strsplit(p, male, perl = TRUE)[[1]])
      conf <- fem / (fem + mal)
      data.frame(
        name = term,
        guess = if (fem > mal) "female" else "male",
        confidence = if (fem > mal) conf else 1 - conf,
        female = fem,
        male = mal
      )
    } else if (attempt > 0 || req$status_code %in% c(503, 999)) {
      sys.sleep(5)
      search(term, attempt = attempt - 1)
    }
  }
  cores <- max(1, min(cores, length(names)))
  res <- if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))
    parLapply(cl, names, search)
  } else {
    lapply(names, search)
  }
  do.call(rbind, res)
}
