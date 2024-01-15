#' BabelNet to WordNet
#'
#' Retrieve [WordNet](https://wordnet.princeton.edu) counterparts to
#' [BabelNet](https://babelnet.org/) IDs where available.
#' @param ids A vector of BabelNet IDs (e.g., \code{"bn:00103811a"}).
#' @param outFile Path of a file to save results to.
#' @param cache Path to a directory in which to save original BabelNet and WordNet files.
#' @param wordnet_dir Path to a WordNet directory (the unpacked tars), ultimately
#' containing a \code{/dict/index.sense} file.
#' @param download_wordnet Logical; if \code{TRUE}, will download the 3.0 version of WordNet,
#' if \code{wordnet_dir} does not exist.
#' @param wordnet_version Version of WordNet pointed to by \code{wordnet_dir},
#' which determines which \code{old_keys} entry is looked for (e.g., \code{30} for version 3.0).
#' @returns A \code{data.frame} with a map between provided \code{ids} and WordNet information.
#' @examples
#' \dontrun{
#' babelnet_to_wordnet("bn:00103811a")
#' }
#' @export

babelnet_to_wordnet <- function(ids, outFile = NULL,
                                cache = paste0(dirname(tempdir()), "/babelnet_sparql/"),
                                wordnet_dir = "~/WordNet-3.0", download_wordnet = FALSE, wordnet_version = 30) {
  if (!is.character(cache)) cache <- tempdir()
  cache <- paste0(normalizePath(cache, "/", FALSE), "/")
  dir.create(cache, FALSE)
  if (download_wordnet && !dir.exists(wordnet_dir)) {
    dir.create(wordnet_dir, FALSE)
    zipfile <- paste0(wordnet_dir, ".tar.gz")
    if (!file.exists(zipfile)) download.file("https://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.gz", zipfile)
    system2("gzip", c("-d", shQuote(normalizePath(zipfile))))
    untar(sub(".gz", "", zipfile, fixed = TRUE), exdir = dirname(wordnet_dir))
  }
  get_local <- FALSE
  if (is.character(wordnet_dir) && dir.exists(wordnet_dir)) {
    op <- paste0(wordnet_dir, "/dict/index.sense")
    if (file.exists(op)) {
      get_local <- TRUE
      index <- arrow::read_delim_arrow(op, delim = " ", col_names = c("key", "id", "i", "j"), col_types = "ccii")
    } else {
      warning("WordNet directory exists, but the sense index is not in the expected location")
    }
  }
  old_version <- paste0("pwn", wordnet_version)
  res <- list()
  for (bid in unique(ids)) {
    if (grepl("bn:", bid, fixed = TRUE)) {
      n <- sub("bn:", "s", bid, fixed = TRUE)
      f <- paste0(cache, n, ".xml")
      ff <- paste0(cache, n, ".json")
      if (!file.exists(f)) {
        req <- httr::GET(paste0("http://babelnet.org/rdf/data/", n), httr::write_disk(f, TRUE))
        if (req$status_code != 200) unlink(f)
      }
      if (file.exists(f)) {
        wd <- NULL
        if (file.exists(ff)) {
          wd <- jsonlite::read_json(ff)
        } else {
          url <- grep("edu/wn", readLines(f, warn = FALSE), fixed = TRUE, value = TRUE)
          if (length(url)) {
            wid <- regmatches(url, regexec("wn31/\\d([^/]+)>", url))[[1]]
            if (length(wid)) {
              wd <- jsonlite::read_json(paste0("http://wordnet-rdf.princeton.edu/json/id/", wid[2]))
              wd[[1]]$babelnet <- bid
              jsonlite::write_json(wd, ff, auto_unbox = TRUE)
            }
          }
        }
        if (get_local && old_version %in% names(wd[[1]]$old_keys)) {
          su <- index$id == substring(wd[[1]]$old_keys[[old_version]][[1]], 1, 8)
          if (any(su)) wd[[1]][[old_version]] <- index$key[su]
        }
        res[[bid]] <- wd[[1]]
      } else {
        warning("failed to download BabelNet file ", paste0("http://babelnet.org/rdf/data/", n))
      }
    } else {
      warning(bid, " is not in the expected format (e.g., bn:00103811a)")
    }
  }
  match_old <- function(ids, term) {
    res <- ids[sub("%.*$", "", ids) == gsub(" ", "_", term, fixed = TRUE)]
    if (length(res)) res else NA
  }
  btow <- do.call(rbind, lapply(ids, function(id) {
    if (id %in% names(res)) {
      m <- res[[id]]
      do.call(rbind, lapply(m$lemmas, function(l) {
        term <- sub("\\([^)]+\\)", "", l$lemma[[1]])
        data.frame(
          id = m$id,
          babelnet = m$babelnet,
          subject = m$subject,
          definition = m$definition,
          term = term,
          wordnet = l$sense_key[[1]],
          wordnet30 = if (length(m[[old_version]])) match_old(m[[old_version]], term) else NA
        )
      }))
    } else {
      data.frame(
        id = id,
        babelnet = NA,
        subject = NA,
        definition = NA,
        term = NA,
        wordnet = NA,
        wordnet30 = NA
      )
    }
  }))
  if (!is.null(outFile)) write.csv(btow, outFile, row.names = FALSE)
  btow
}
