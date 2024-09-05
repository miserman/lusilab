#' Reddit
#'
#' In parts, a wrapper for the RedditExtractoR package; pulls posts and comments
#' from specified subreddits or searches.
#'
#' @param topics A string or vector of strings corresponding to subreddit names (e.g., 'trees'
#' referring to reddit.com/r/trees/). Only the first value is used if search is specified.
#' @param search Passed to \code{\link[RedditExtractoR]{find_thread_urls}} as the search_terms argument.
#' If this is specified, if topic is specified, the first topic value will be added as the
#' subreddit argument (which will restrict search to that subreddit).
#' @param ... Passed additional arguments to \code{\link[RedditExtractoR]{find_thread_urls}} if search
#' is specified.
#' @param sort How to sort initial comments. Only applies if search is not specified. Default is
#' 'hot', with 'new', 'rising', 'top', 'gilded', and 'ads', as options.
#' @param filename Name of the file to be saved in the current working directory. This will
#' currently always be a csv file.
#' @param write Logical: if FALSE, data will not be save to a file (they will just be stored as
#' objects if you've named your reddit call).
#' @param lim Numeric: sets the number of posts to pull per topic. Only applies if \code{search}
#' is not specified.
#' @param filter Passed to \code{\link{grepl}}. A pattern used to filter posts by the content of
#' their comments. default is '\\[removed\\]' to filter out those comments that have been deleted.
#' @param clean Logical; if \code{FALSE}, converts curly to straight quotes.
#' @param comments_only Logical; if \code{TRUE}, will exclude original posts from each thread.
#' @param posts_only Logical; if \code{TRUE}, will only collect the initial post for each thread.
#' This changes what is returned, relative to posts returned along with comments.
#' @param useragent String to set as the request's User-Agent.
#' @param users A vector of user names (as in reddit.com/user/username; such as those in the user
#' column from the reddit function). Information is never gathered twice for the same user;
#' \code{\link{reddit.usercomments}} simply removes duplicate user names (i.e., unique(users)),
#' whereas reddit.karma will return karma scores in the same order as the input, filling in the
#' same information for duplicate users.
#' @param subreddits A vector of subreddits to filter for; only comments within the specified
#' subreddits are returned. Should exactly match the subreddit_name_prefix field (e.g., 'r/Anxiety'
#' for \href{reddit.com/r/Anxiety}{https://www.reddit.com/r/Anxiety/}), including 'r/' before each
#' subreddit name, though this will be added if missing.
#' @param type Type of user data to download; either 'comments' or 'submissions' (posts).
#' @param data The \code{data.frame} returned from a reddit call (e.g.,
#' \code{comments = reddit('trees'); reddit.lsm(comments)})
#' @returns From \code{reddit} if \code{posts_only} is \code{FALSE}:
#' A \code{data.frame} with a row for each post / comment:
#' \itemize{
#'    \item \strong{\code{url}}: URL of the thread.
#'    \item \strong{\code{author}}: Username of the author.
#'    \item \strong{\code{date}}: Date of the submission in YYYY-MM-DD format.
#'    \item \strong{\code{timestamp}}: Time of the submission in seconds since 1970-01-01;
#'      use \code{\link{as.POSIXct}} to convert to a date object.
#'    \item \strong{\code{score}}: Score.
#'    \item \strong{\code{upvotes}}: Number of downvotes.
#'    \item \strong{\code{downvotes}}: Number of upvotes.
#'    \item \strong{\code{golds}}: Number of golds.
#'    \item \strong{\code{comment}}: Text of the submission. Called \code{comment} for
#'      compatibility, though it may be a post or comment, as indicated by \code{type}.
#'    \item \strong{\code{comment_id}}: Position of the submission within the thread, where 0
#'      means original post.
#'    \item \strong{\code{type}}: Whether the submission is a comment or post.
#' }
#' If \code{posts_only} is \code{TRUE}, a \code{data.frame} with all variables from the
#' Reddit response, where, for instance, text will be \code{selftext}.
#' @examples
#' \dontrun{
#' # these will all save a file called 'reddit.csv' to the current working directory.
#' # pulls from a single, depression related subreddit:
#' reddit("depression")
#'
#' # pull from a few subreddits, also saving the data as an object ('comments'):
#' topics <- c("trees", "Meditation")
#' comments <- reddit(topics)
#'
#' # pull comments from a search
#' reddit(search = "politics")
#'
#' # calculate language style matching between each comment and the comment it's replying to
#' # within the first thread of the trees subreddit
#' thread_lsm <- reddit.lsm(comments[comments$title == comments$title[1], ])
#'
#' # download the 5 most recent comments from 10 users who commented in the trees subreddit
#' user_comments <- reddit.usercomments(comments$user[1:5], lim = 5)
#' }
#' @export
#'

reddit <- function(topics, search = NULL, ..., sort = "hot", filename = "reddit.csv", write = TRUE,
                   lim = 100, filter = "\\[removed\\]", clean = TRUE, comments_only = FALSE,
                   posts_only = FALSE, useragent = paste("R LUSI @", date())) {
  saf <- options("stringsAsFactors")[[1]]
  options(stringsAsFactors = FALSE)
  on.exit(options(stringsAsFactors = saf))
  if (!is.null(search)) {
    cat("collecting urls from search...\n")
    tl <- tryCatch(
      RedditExtractoR::find_thread_urls(keywords = search, subreddit = if (is.null(topics)) NA else topics[1], ...)$url,
      error = function(e) stop(e$message, call. = FALSE)
    )
    topics <- 1
  }
  sop <- c("hot", "new", "rising", "top", "gilded", "ads")
  sort <- sop[pmatch(tolower(sort), sop)]
  d <- NULL
  ua <- httr::user_agent(useragent)
  for (t in topics) {
    if (is.null(search)) {
      thread_lim <- lim
      bl <- paste0("https://www.reddit.com/r/", t, "/")
      tl <- NULL
      after <- ""
      cat(paste0("collecting posts from /r/", t, "... "))
      while (thread_lim > 0) {
        req <- httr::GET(paste0(bl, ".json?limit=", thread_lim, "&after=", after), ua)
        if (req$status_code != 200) {
          content <- tryCatch(fromJSON(rawToChar(req$content)), error = function(e) NULL)
          warning(paste(
            "failed to retrieve posts from ", t,
            if (is.list(content)) paste0("(", content$error, "): ", content$message)
          ))
          break
        }
        content <- tryCatch(fromJSON(rawToChar(req$content)), error = function(e) NULL)
        if (is.null(content)) {
          thread_lim <- 0
          next
        } else {
          tl <- unique(c(tl, paste0(
            bl, content$data$children$data$id, ".json", if (sort != "hot") paste0("?sort=", sort)
          )))
        }
        thread_lim <- thread_lim - nrow(content$data$children$data)
        if (posts_only) {
          nr <- nrow(content$data$children$data)
          if (!is.null(nr) && nr) {
            content$data$children$data[, vapply(content$data$children$data, is.list, TRUE)] <- NA
            cn <- colnames(content$data$children$data)
            if (!is.null(d)) {
              su <- colnames(d) %in% cn
              if (!all(su)) content$data$children$data[, colnames(d)[!su]] <- NA
              su <- cn %in% colnames(d)
              if (!all(su)) content$data$children$data <- content$data$children$data[, su]
            }
            d <- rbind(d, content$data$children$data)
          } else {
            thread_lim <- 0
          }
        }
        if (!is.null(content$data$after)) {
          after <- content$data$after
        } else {
          thread_lim <- 0
        }
      }
      cat(length(tl), "found\n")
    }
    if (!posts_only) {
      cat(paste0("collecting comments from ", if (is.null(search)) paste0("/r/", t) else paste("search:", search), "\n"))
      for (p in seq(tl)) {
        cat(paste(p, "/", length(tl), ":", gsub("\\.json.*$|\\?ref.*$", "/", tl[p]), "\n"))
        d <- rbind(d, tryCatch(
          {
            thread <- RedditExtractoR::get_thread_content(sub("\\.json.*", "", tl[p]))
            thread$comments$type <- "comment"
            if (comments_only) {
              return(thread$comments)
            }
            post <- thread$threads[c(
              "url", "author", "date", "timestamp", "score", "upvotes", "downvotes", "golds"
            )]
            post$comment <- thread$threads$text
            post$comment_id <- 0
            post$type <- "post"
            rbind(post, thread$comments)
          },
          error = function(e) NULL
        ))
      }
    }
  }
  if (posts_only) {
    if (!is.null(d) && clean) d$selftext <- gsub("[\034\035]", '"', gsub("[\030\031]", "'", d$selftext))
  } else {
    d <- d[!grepl(filter, d$comment), ]
    if (clean) d$comment <- gsub("[\034\035]", '"', gsub("[\030\031]", "'", d$comment))
  }
  if (write) {
    message(paste0("output saved to ", getwd(), "/", filename))
    write.csv(d, filename, row.names = FALSE)
  }
  invisible(d)
}

#' @rdname reddit
#' @export

reddit.karma <- function(users) {
  uu <- unique(users)
  pu <- function(u) {
    p <- tryCatch(readLines(paste0("https://old.reddit.com/user/", u), warn = FALSE), error = function(e) NULL)
    if (!is.null(p)) {
      p <- p[grep("karma", p)[1]]
      p <- strsplit(p, if (any(grepl('class="karma"', p, fixed = TRUE))) 'karma"' else " Karma", fixed = TRUE)[[1]]
      p <- if (length(p) == 4) {
        p <- p[1:3]
        vapply(regmatches(p, regexec(">[^>]*$", p)), function(s) as.numeric(gsub("[^0-9]", "", s)), 0)
      } else {
        p <- p[2:3]
        p <- as.numeric(gsub("[^0-9]", "", sub("<.*$", "", p)))
        c(p[1] + p[2], p)
      }
    } else {
      c(NA, NA, NA)
    }
  }
  if (length(uu) < length(users)) {
    res <- data.frame(User = users, Total = NA, Post = NA, Comment = NA)
    for (u in uu) {
      su <- users == u
      k <- pu(u)
      rs <- sum(su)
      res[su, -1] <- if (rs == 1) k else matrix(rep(k, rs), ncol = 3, byrow = TRUE)
    }
  } else {
    res <- data.frame(t(vapply(users, pu, c(0, 0, 0))))
    res <- cbind(as.character(users), res)
    colnames(res) <- c("User", "Total", "Post", "Comment")
  }
  res
}

#' @rdname reddit
#' @export

reddit.usercomments <- reddit.userdata <- function(
    users, filename = NULL, subreddits = NULL, lim = 100, type = "comments",
    useragent = paste("R LUSI @", date())) {
  if (!missing(type)) type <- if (grepl("^c", type)) "comments" else "submitted"
  if (!is.null(subreddits) && any(!grepl("r/", subreddits, fixed = TRUE))) subreddits <- sub("^r/|^", "r/", subreddits)
  ua <- httr::user_agent(useragent)
  d <- list()
  for (u in unique(users)) {
    a <- ""
    op <- NULL
    while (lim > 0) {
      tt <- httr::GET(paste0(
        "https://www.reddit.com/user/",
        u, "/", type, "/.json?limit=", lim, "&after=", a
      ), ua)
      if (tt$status_code != 200) {
        tt <- tryCatch(fromJSON(rawToChar(tt$content)), error = function(e) NULL)
        warning(paste(
          "failed to retrieve content for user", u, if (is.list(tt)) paste0("(", tt$error, "): ", tt$message)
        ))
        break
      }
      tt <- tryCatch(fromJSON(rawToChar(tt$content)), error = function(e) NULL)
      nr <- nrow(tt$data$children$data)
      if (!is.null(nr) && nr) {
        tt$data$children$data[, vapply(tt$data$children$data, is.list, TRUE)] <- NA
        cn <- colnames(tt$data$children$data)
        if (!is.null(op)) {
          su <- colnames(op) %in% cn
          if (!all(su)) op <- op[, su]
          su <- cn %in% colnames(op)
          if (!all(su)) tt$data$children$data <- tt$data$children$data[, su]
        }
        op <- rbind(op, tt$data$children$data)
        if (!is.null(tt$data$after)) {
          a <- tt$data$after
          lim <- lim - nrow(tt$data$children$data)
        } else {
          lim <- 0
        }
      } else {
        lim <- 0
      }
    }
    d[[u]] <- op
  }
  if (length(d)) {
    if (length(d) != 1) {
      cols <- lapply(d, colnames)
      if (any(vapply(cols, length, 0) != length(cols[[1]]))) {
        common <- Reduce(intersect, cols)
        for (i in seq_along(d)) d[[i]] <- d[[i]][, common]
      }
    }
    d <- do.call(rbind, unname(d))
    if (!is.null(subreddits)) d <- d[d$subreddit_name_prefixed %in% subreddits, ]
    if (!is.null(filename)) {
      write.csv(d, filename, row.names = FALSE)
      message("file saved to ", paste0(getwd(), "/", filename))
    }
    d
  }
}

#' @rdname reddit
#' @export

reddit.lsm <- function(data) {
  data$post_text <- do.call(paste, lapply(data[, c("title", "post_text")], as.character))
  data$post_text <- gsub("\031", "'", data$post_text, fixed = TRUE)
  data$title <- gsub("\031", "'", as.character(data$title), fixed = TRUE)
  data$comment <- gsub("\031", "'", data$comment, fixed = TRUE)
  ptxt <- unique(data$title)
  posts <- data.frame(lma_termcat(lma_weight(lma_dtm(ptxt))))
  if (ncol(posts) == 1) posts <- data.frame(t(posts))
  rownames(posts) <- ptxt
  thread <- as.data.frame(t(vapply(as.character(data$structure), function(id) {
    s <- strsplit(id, "_(?=\\d+$)", perl = TRUE)[[1]]
    if (length(s) == 1) s <- c(0, s)
    c(conv = s[1], index = s[2])
  }, c("", ""))), row.names = FALSE)
  thread$title <- data$title
  thread$lsm <- NA
  for (p in ptxt) {
    tryCatch(
      {
        su <- thread$title == p
        d <- data[su, ]
        if (nrow(d) == 1) {
          thread[su, "lsm"] <- lingmatch(data[su, "comment"], data[su, "post_text"], type = "lsm", drop = FALSE)$sim
          next
        }
        comments <- lma_termcat(lma_weight(lma_dtm(d$comment)))
        comments <- cbind(thread[thread$title == p, ], comments)
        nc <- ncol(comments) - 4
        uc <- as.character(unique(comments$conv))
        cs <- t(vapply(uc[-1], function(r) {
          tsu <- if (grepl("_", r, fixed = TRUE)) {
            with(comments, conv == (h <- strsplit(r, "_(?=\\d+$)",
              perl = TRUE
            )[[1]])[1] & index == h[2])
          } else {
            with(comments, conv == 0 & index == r)
          }
          if (sum(tsu) == 0) as.list(rep(NA, nc)) else comments[tsu, -(1:4)]
        }, as.list(numeric(nc))))
        if (is.null(colnames(cs))) colnames(cs) <- colnames(posts)
        comp <- data.frame(conv = uc, title = p, rbind(posts[p, ], cs))
        thread[su, "lsm"] <- if (all(comments$conv == 0)) {
          lingmatch(comments[, -(1:4)], comp, type = "lsm", drop = FALSE)$sim[, 1]
        } else {
          lingmatch(comments, comp, group = "conv", type = "lsm", drop = FALSE)$sim[, 2]
        }
      },
      error = function(e) NULL
    )
  }

  thread
}
