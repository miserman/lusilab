#' BabelNet Synset
#'
#' Retrieve [BabelNet](https://babelnet.org/) synset information.
#' @param ids A vector of BabelNet IDs (e.g., \code{"bn:00103811a"}).
#' @param outDir Path of a directory in which to save parsed page JSON files.
#' @param overwrite Logical; if \code{TRUE}, will overwrite existing parsed pages when retrieved.
#' @param cache Path to a directory in which to save original page HTML files.
#' @param cores Number of CPU cores to use during processing
#' @returns A list with entries for each id in \code{ids}.
#' @examples
#' bablenet_synset("bn:00103811a")
#' @export

bablenet_synset <- function(ids, outDir = NULL, overwrite = FALSE,
                            cache = paste0(dirname(tempdir()), "/babelnet_pages/"), cores = parallel::detectCores() - 2) {
  if (!is.character(cache)) cache <- tempdir()
  dir.create(cache, FALSE, TRUE)
  if (!is.null(outDir)) dir.create(cache, FALSE, TRUE)

  parse_tabs <- function(content) {
    parts <- strsplit(content, '<a class="source"[^>]+>')
    parts <- parts[vapply(parts, length, 0) > 1]
    content <- vapply(parts, "[[", "", 1)
    content <- gsub("^[^<]+>|[\\t\\n\\s]+", " ", content, perl = TRUE)
    content <- gsub("<[^>]*>", "", content)
    content <- gsub("^\\s+|\\s\\s+|\\s+$", "", content)
    data.frame(
      source = gsub("<[^$|]+", "", vapply(
        parts, function(p) paste(p[seq(2, length(p))], collapse = "|"), ""
      )),
      content = content
    )
  }

  processor <- function(id) {
    file_id <- sub(":", "", id)
    if (!is.null(outDir)) {
      out_file <- paste0(outDir, "/", file_id, ".json")
      if (overwrite) unlink(out_file)
      if (file.exists(out_file)) {
        return(jsonlite::read_json(out_file))
      }
    }
    res <- list(id = id, lemmas = NULL, definitions = NULL, examples = NULL, relations = NULL, sources = NULL)
    page <- NULL
    file <- paste0(cache, file_id, ".rds")
    if (file.exists(file)) {
      page <- readRDS(file)
    } else {
      url <- paste0("https://babelnet.org/synset?id=", id, "&lang=EN")
      req <- httr::GET(url)
      if (req$status_code == 200) {
        page <- rawToChar(req$content)
        saveRDS(page, file, compress = "xz")
      } else {
        unlink(file)
      }
    }
    if (!is.null(page)) {
      tryCatch(
        {
          lemmas <- strsplit(page, '<div class="synonim-list"', fixed = TRUE)[[1]][[2]]
          lemmas <- strsplit(lemmas, '<div class="image-and-definition">', fixed = TRUE)[[1]][[1]]
          lemmas <- gsub("&nbsp;|[\\n\\t]+|\\s\\s+", "", lemmas, perl = TRUE)
          res$lemmas <- regmatches(lemmas, gregexec(">([^>]+)<", lemmas))[[1]][2, ]
          tabs <- strsplit(page, '<div class="tab" data-tab="', fixed = TRUE)[[1]]
          names(tabs) <- substring(tabs, 1, 7)
          if ("definit" %in% names(tabs)) {
            res$definitions <- parse_tabs(
              strsplit(tabs["definit"], '<div class="definition"', fixed = TRUE)[[1]]
            )
          }
          if ("example" %in% names(tabs)) {
            res$examples <- parse_tabs(
              strsplit(tabs["example"], '<div class="example"', fixed = TRUE)[[1]]
            )
          }
          if ("relatio" %in% names(tabs)) {
            relations <- strsplit(tabs["relatio"], '<div class="relation"', fixed = TRUE)[[1]]
            relations <- strsplit(relations, '<div class="type" title="', fixed = TRUE)[[1]]
            relations <- relations[seq(2, length(relations))]
            res$relations <- structure(lapply(
              relations,
              function(e) {
                entries <- strsplit(e, '<a class="relation"', fixed = TRUE)[[1]]
                entries <- as.data.frame(do.call(rbind, lapply(
                  strsplit(entries[seq(2, length(entries))], 'data-synset-id="|synonim-wrapper\">'),
                  function(p) sub('["<].*$', "", p[2:3])
                )))
                colnames(entries) <- c("id", "term")
                entries
              }
            ), names = sub('".*$', "", relations))
          }
          if ("sources" %in% names(tabs)) {
            sources <- strsplit(
              strsplit(tabs["sources"], "<!-- Modal Preferences -->", fixed = TRUE)[[1]][[1]],
              '<div class="title">',
              fixed = TRUE
            )[[1]]
            sources <- sources[seq(2, length(sources))]
            res$sources <- structure(
              lapply(sources, function(e) {
                e <- strsplit(e, '<a class=\"synonim\" data-lemma-type=\"', fixed = TRUE)[[1]]
                e <- e[seq(2, length(e))]
                e <- as.data.frame(do.call(rbind, lapply(strsplit(e, '["<][^">]+[">]'), "[", c(4, 1, 2))))
                colnames(e) <- c("name", "type", "url")
                e
              }),
              names = sub("<.*$", "", sources)
            )
          }
        },
        error = function(e) warning("failed to parse ", id)
      )
    }
    if (!is.null(outDir)) jsonlite::write_json(res, out_file, auto_unbox = TRUE)
    res
  }

  all_res <- if (cores > 1 && length(ids) > 1) {
    cl <- parallel::makeCluster(min(length(ids), cores))
    on.exit(parallel::stopCluster(cl))
    parallel::parLapply(cl, ids, processor)
  } else {
    lapply(ids, processor)
  }
  names(all_res) <- ids

  invisible(all_res)
}
