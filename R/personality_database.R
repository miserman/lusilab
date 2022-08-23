#' Personality-Database API
#'
#' Collect data from the \href{https://www.personality-database.com}{personality-database} API.
#'
#' @param profiles Name of the profile list, between \code{famous}, \code{fictional}, and \code{discovery}.
#' @param category Name of the profile category, as listed under each list, or \code{all} (default).
#' @param outFile Path to a .csv file to save results as.
#' @param sort How the profile list should be sorted, between \code{hot} (default), \code{new}, \code{top},
#' and \code{alphabet}.
#' @param start Profile from the list to start on.
#' @param limit Maximum number of profiles to include.
#' @param retry Logical; if \code{TRUE}, will retry after a failed attempt.
#' @param verbose Logical; if \code{FALSE}, will not display status messages.
#' @param cores Number of CPU cores to split \code{names} across.
#' @returns a \code{data.frame} with entries for \code{name} (the original search term),
#' \code{guess}, \code{confidence}, and \code{female} and \code{male} with count of matches.
#' @examples
#' \dontrun{
#' # get scores from the 5 "HOT"est famous people
#' hot5 <- personality_database(limit = 5)
#' }
#' @export
#'

personality_database <- function(profiles = "famous", category = "all", outFile = NULL, sort = "hot", start = 0,
                                 limit = Inf, retry = 10, verbose = TRUE, cores = detectCores() - 1) {
  if (!is.null(outFile) && file.exists(outFile)) {
    return(read.csv(outFile))
  }
  struct <- GET("https://api.personality-database.com/api/v1/pcategories")
  if (struct$status_code != 200) stop("failed to connect to API", call. = FALSE)
  struct <- fromJSON(rawToChar(struct$content))
  pid <- structure(struct$property_id, names = tolower(substr(struct$property, 1, 2)))[tolower(substr(profiles, 1, 2))]
  if (is.na(pid)) stop(profiles, " is not a recognized set of profiles", call. = FALSE)
  cats <- struct$categories[[1]]
  cid <- c(all = 0, structure(cats$cat_id, names = tolower(substr(sub("^.* ", "", cats$category), 1, 3))))[tolower(substr(category, 1, 3))]
  if (is.na(cid)) stop(category, " is not a recognized category", call. = FALSE)
  sort <- match.arg(sort, c("hot", "new", "top", "alphabet"))
  if (verbose) message("retrieving profile list: ", struct[pid, "property"], " > ", if (cid == 0) "all" else cats[cats$cat_id == cid, "category"])
  perpage <- max(20, min(500, limit))
  offset <- max(0, floor(start / perpage))
  profiles <- GET(paste0(
    "https://api.personality-database.com/api/v1/profiles?pid=",
    pid, "&property_id=", pid, "&cid=", cid, "&cat_id=", cid, "&offset=", offset, "&limit=", perpage, "&sort=", sort
  ))
  if (profiles$status_code != 200) stop("failed to retrieve profile list", call. = FALSE)
  profiles <- fromJSON(rawToChar(profiles$content))$profiles
  if (limit > perpage && nrow(profiles) < perpage) {
    while (nrow(profiles) >= perpage) {
      offset <- offset + nrow(profiles)
      next_page <- GET(paste0(
        "https://api.personality-database.com/api/v1/profiles?pid=",
        pid, "&property_id=", pid, "&cid=", cid, "&cat_id=", cid, "&offset=", offset, "&limit=", perpage, "&sort=", sort
      ))
      if (next_page$status_code == 200) {
        next_page <- fromJSON(rawToChar(next_page$content))$profiles
        if (length(next_page)) {
          profiles <- rbind(profiles, next_page)
        } else {
          break
        }
      } else {
        break
      }
    }
  }
  if (nrow(profiles) > limit) profiles <- profiles[seq_len(limit), ]
  if (verbose) message("retrieving measures from ", nrow(profiles), " profiles")
  cache <- paste0(tempdir(), "/personality_database/")
  dir.create(cache, FALSE)
  cores <- max(1, min(cores, nrow(profiles)))
  retrieve <- function(id, attempt = 1) {
    cached <- paste0(cache, id, ".json")
    res <- if (file.exists(cached)) {
      jsonlite::read_json(cached, simplifyVector = TRUE)
    } else {
      req <- httr::GET(paste0("https://api.personality-database.com/api/v1/profile/", id), httr::write_disk(cached))
      if (req$status_code == 200) {
        jsonlite::read_json(cached, simplifyVector = TRUE)
      } else if (attempt < retry) {
        Sys.sleep(1)
        return(retrieve(id, attempt + 1))
      }
    }
    if (!is.null(res)) {
      systems <- structure(gsub("[ ()]+", "_", sub("\\)$", "", tolower(res$systems$system_name))), names = res$systems$id)
      as.data.frame(c(
        Filter(length, list(
          id = res$id,
          name = res$mbti_profile,
          category = res$category,
          subcategory = res$subcategory,
          fictional = res$category_is_fictional,
          description = res$wiki_description,
          watchers = res$watch_count,
          comments = res$comment_count,
          updated = res$type_updated_date,
          votes = res$vote_count,
          votes_total = res$total_vote_counts,
          votes_enneagrams = res$vote_count_enneagram,
          votes_mbti = res$vote_count_mbti
        )),
        unlist(lapply(unname(res$breakdown_systems), function(s) {
          if (any(s$theCount != 0)) {
            ns <- paste0(
              if (s[1, "personality_system_id"] %in% names(systems)) paste0(systems[[as.character(s[1, "personality_system_id"])]], "."),
              gsub("[ /[]+", "_", sub("\\]$", "", tolower(s$personality_type)))
            )
            structure(s$theCount, names = ns)
          }
        })),
        structure(res$mbti_letter_stats$PercentageFloat, names = paste0("letter_stats.", res$mbti_letter_stats$type))
      ))
    }
  }
  res <- if (cores > 1) {
    ids <- profiles$profile_id
    env <- new.env(parent = globalenv())
    environment(retrieve) <- env
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))
    for (x in c("cl", "ids", "retrieve", "cache", "retry")) {
      env[[x]] <- get(x)
    }
    eval(expression(parallel::parLapply(cl, ids, retrieve)), envir = env)
  } else {
    lapply(profiles$profile_id, retrieve)
  }
  cols <- unique(unlist(lapply(res, colnames), use.names = FALSE))
  cols <- c(cols[1:13], sort(cols[-(1:13)]))
  final <- do.call(rbind, lapply(res, function(r) {
    su <- !cols %in% colnames(r)
    if (any(su)) {
      r <- cbind(r, as.list(structure(rep(0, sum(su)), names = cols[su])))
    }
    r[, cols]
  }))
  if (!is.null(outFile)) {
    dir.create(dirname(outFile), FALSE)
    write.csv(final, outFile, row.names = FALSE)
  }
  final
}
