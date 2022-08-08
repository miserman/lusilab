#' Predict Demographics From Name
#'
#' Predict sex and maybe country from a name.
#'
#' @param given A vector of given (first) names. Case sensitive for all but \code{wgnd} and \code{fb_scraped}.
#' @param family A vector of family (last; sur) names. Only used in \code{fb} to adjust country predictions. Case sensitive.
#' @param country A vector of 2-letter country codes. Only used in \code{wgnd} to give \code{sex_in_country_wgnd}.
#' @param source A vector specify which source(s) to use:
#' \itemize{
#'   \item \href{https://dataverse.harvard.edu/dataverse/WGND}{World Gender Name Dictionary} (\code{wgnd})
#'   \item \href{https://github.com/philipperemy/name-dataset}{Facebook Hack} (\code{fb})
#'   \item \href{https://sites.google.com/site/facebooknamelist/home}{Facebook Scraped} (\code{fb_scraped})
#'   \item \href{https://archive.ics.uci.edu/ml/datasets/Gender+by+Name#}{Skydeck Censuses} (\code{skydeck})
#'   \item \href{https://www.ssa.gov/OACT/babynames/limits.html}{U.S. Social Security Administration Baby Names} (\code{usssa})
#' }
#' Defaults to \code{all} to return all.
#' @param dir Directory in which to save original and prepared names data.
#' @param ssa_source Source of USSSA data; passed to \code{\link{get_baby_names}}.
#' @param full_country Logical; if \code{TRUE}, a column for each country will be returned for source
#' \code{fb}, each containing a probability for that country. Otherwise, only the most likely country
#' is returned in a \code{predicted_country} column.
#' @param verbose Logical; if \code{FALSE}, will not show status messages.
#'
#' @returns A \code{data.frame} with columns for \code{given}, \code{family}, and \code{country} (as provided),
#' and columns for each \code{source}, including \code{count} and \code{prob_fem} prefixed by the source code,
#' and for \code{fb}, either \code{predicted_country_fb}, or a set of country codes prefixed by \code{_fb}.
#'
#' @examples
#' \dontrun{
#'
#' # saves to a temporary directory
#' predict_demographics(c("Jane", "John"), c("Doe", "Doe"))
#' }
#' @export

predict_demographics <- function(given, family = NULL, country = NULL, source = "all", dir = tempdir(),
                                 ssa_source = "national", full_country = FALSE, verbose = TRUE) {
  sources <- list(
    wgnd = list(
      title = "World Gender Name Dictionary",
      source = "https://dataverse.harvard.edu/dataverse/WGND",
      url = "https://dataverse.harvard.edu/api/access/datafile/4750350",
      original = "wgnd2.tab",
      final = "wgnd_db",
      year = 2021
    ),
    fb = list(
      title = "Facebook Hack",
      source = "https://github.com/philipperemy/name-dataset",
      url = c(
        "https://raw.githubusercontent.com/philipperemy/name-dataset/master/names_dataset/v3/first_names.zip",
        "https://raw.githubusercontent.com/philipperemy/name-dataset/master/names_dataset/v3/last_names.zip"
      ),
      original = c("first_names.zip", "last_names.zip"),
      final = c("fb_given_db", "fb_family_db"),
      year = 2021
    ),
    fb_scraped = list(
      title = "Facebook Scraped",
      source = "https://sites.google.com/site/facebooknamelist/home",
      url = "https://sites.google.com/site/facebooknamelist/namelist/firstname_nickname.csv?attredirects=0&d=1",
      original = "facebook_scaped.csv",
      final = "facebook_scaped_prepared.csv",
      year = 2009
    ),
    skydeck = list(
      title = "Skydeck Censuses",
      source = "https://archive.ics.uci.edu/ml/datasets/Gender+by+Name",
      url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00591/name_gender_dataset.csv",
      original = "skydeck.csv",
      final = "skydeck_prepared.csv",
      year = 2019
    ),
    usssa = list(
      title = "U.S. Social Security Administration Baby Names",
      source = "https://www.ssa.gov/OACT/babynames/limits.html",
      final = "ssa_prepared.csv",
      year = 2021
    )
  )
  dir <- paste0(normalizePath(dir, "/"), "/")
  source <- tolower(source)
  if (source == "all") source <- names(sources)
  requested <- structure(names(sources) %in% source, names = names(sources))
  if (!any(requested)) stop("source not recognized; should be any of ", paste(names(sources), collapse = ", "))
  for (rs in names(which(requested))) {
    if (rs != "ssa") {
      s <- sources[[rs]]
      dir.create(dir, FALSE, TRUE)
      if (!all(file.exists(paste0(dir, s$original))) && !file.exists(paste0(dir, s$final))) {
        if (verbose) message("downloading dataset:\n  ", s$title, "\n  ", s$source, "\n")
        for (i in seq_along(s$original)) download.file(s$url[[i]], paste0(dir, s$original[[i]]), quiet = !verbose)
      }
    }
  }
  if (!is.null(country)) {
    if (anyNA(country)) country[is.na(country)] <- ""
    if (any(nchar(country) > 2)) stop("country should be 2 character codes")
    country <- toupper(country)
  }
  if (!is.null(given)) {
    if (anyNA(given)) given[is.na(given)] <- ""
    is_lowercase <- !grepl("^[A-Z]", given, perl = TRUE)
    if (any(is_lowercase)) given[is_lowercase] <- gsub("\\b(\\w)", "\\U\\1", given[is_lowercase], perl = TRUE)
    given[is.na(given) | given == ""] <- " "
  }
  start <- name <- gender <- NULL
  ogiven <- given
  res <- data.frame(given = unique(given), family = "", country = "")
  given <- res$given
  lgiven <- tolower(given)
  if (requested[["wgnd"]]) {
    db_dir <- paste0(dir, "wgnd_db")
    if (!dir.exists(db_dir)) {
      original <- paste0(dir, sources$wgnd$original)
      if (verbose) message("creating WGND database (takes a while)")
      raw <- arrow::read_csv_arrow(original)
      raw$start <- as.character(vapply(iconv(substring(raw$name, 1, 1), toRaw = TRUE), "[[", raw(1), 1))
      arrow::write_dataset(raw, db_dir, partitioning = "start")
      rm(raw)
    }
    first_raw <- as.character(vapply(iconv(substring(lgiven, 1, 1), toRaw = TRUE), "[[", raw(1), 1))
    db <- arrow::open_dataset(db_dir)
    initial <- dplyr::compute(dplyr::filter(db, start %in% first_raw))
    if (nrow(initial)) {
      matched_wgnd <- dplyr::compute(dplyr::filter(initial, name %in% lgiven))
      if (nrow(matched_wgnd)) {
        mres <- as.data.frame(as.data.frame(dplyr::summarise(
          dplyr::group_by(matched_wgnd, name),
          count = n(), prob_fem = mean(gender == "F")
        )))
        found <- lgiven %in% mres$name
        rownames(mres) <- mres$name
        mres$name <- NULL
        nmissed <- sum(!found)
        if (nmissed) {
          mres <- rbind(
            mres, as.data.frame(matrix(
              rep(c(0, .5), each = nmissed), nmissed,
              dimnames = list(lgiven[!found], colnames(mres))
            ), make.names = FALSE, optional = TRUE)
          )
        }
        colnames(mres) <- paste0(colnames(mres), "_wgnd")
        res <- cbind(res, mres[lgiven, ])
        rownames(res) <- NULL
      }
    }
  }
  if (requested[["fb"]]) {
    final <- paste0(dir, sources$fb$final)
    if (!any(file.exists(final))) {
      if (verbose) message("creating Facebook database (takes a while)")
      vec <- structure(numeric(106), names = c(
        "prob_fem", "AE", "AF", "AL", "AO", "AR", "AT", "AZ", "BD", "BE", "BF", "BG", "BH", "BI", "BN", "BO",
        "BR", "BW", "CA", "CH", "CL", "CM", "CN", "CO", "CR", "CY", "CZ", "DE", "DJ", "DK", "DZ",
        "EC", "EE", "EG", "ES", "ET", "FI", "FJ", "FR", "GB", "GE", "GH", "GR", "GT", "HK", "HN",
        "HR", "HT", "HU", "ID", "IE", "IL", "IN", "IQ", "IR", "IS", "IT", "JM", "JO", "JP", "KH",
        "KR", "KW", "KZ", "LB", "LT", "LU", "LY", "MA", "MD", "MO", "MT", "MU", "MV", "MX", "MY",
        "NA", "NG", "NL", "NO", "OM", "PA", "PE", "PH", "PL", "PR", "PS", "PT", "QA", "RS", "RU",
        "SA", "SD", "SE", "SG", "SI", "SV", "SY", "TM", "TN", "TR", "TW", "US", "UY", "YE", "ZA"
      ))
      if (!file.exists(final[[1]])) {
        unzip(paste0(dir, sources$fb$original[[1]]), exdir = dir)
        firsts <- jsonlite::read_json(paste0(dir, "first_names.json"))
        firsts <- as.data.frame(do.call(rbind, lapply(firsts, function(d) {
          cs <- unlist(d$country)
          vec[names(cs)] <- cs
          probfem <- d$gender[["F"]]
          if (!is.null(probfem)) vec[1] <- probfem
          vec
        })))
        firsts <- cbind(
          start = as.character(vapply(iconv(substring(rownames(firsts), 1, 1), toRaw = TRUE), "[[", raw(1), 1)),
          name = rownames(firsts),
          firsts
        )
        arrow::write_dataset(firsts, final[[1]], partitioning = "start")
        rm(firsts)
      }
      if (!file.exists(final[[2]])) {
        vec <- vec[-1]
        unzip(paste0(dir, sources$fb$original[[2]]), exdir = dir)
        lasts <- jsonlite::read_json(paste0(dir, "last_names.json"))
        lasts <- as.data.frame(do.call(rbind, lapply(lasts, function(d) {
          cs <- unlist(d$country)
          vec[names(cs)] <- cs
          vec
        })))
        lasts <- cbind(
          start = as.character(vapply(iconv(substring(rownames(lasts), 1, 1), toRaw = TRUE), "[[", raw(1), 1)),
          name = rownames(lasts),
          lasts
        )
        arrow::write_dataset(lasts, final[[2]], partitioning = "start")
        rm(lasts)
      }
    }
    if (!is.null(given)) {
      first_raw <- as.character(vapply(iconv(substring(given, 1, 1), toRaw = TRUE), "[[", raw(1), 1))
      db <- arrow::open_dataset(final[[1]])
      initial <- dplyr::compute(dplyr::filter(db, start %in% first_raw))
      if (nrow(initial)) {
        mres <- as.data.frame(dplyr::compute(dplyr::filter(initial, name %in% given)))
        if (nrow(mres)) {
          rownames(mres) <- mres$name
          found <- given %in% mres$name
          mres <- mres[, !colnames(mres) %in% c("name", "start")]
          nmissed <- sum(!found)
          if (nmissed) {
            mres <- rbind(
              mres, as.data.frame(matrix(
                rep(c(.5, numeric(ncol(mres) - 1)), each = nmissed), nmissed,
                dimnames = list(given[!found], colnames(mres))
              ))
            )
          }
          colnames(mres) <- paste0(colnames(mres), "_fb")
          res <- cbind(res, mres[given, ])
        }
      }
    }
  }
  if (requested[["fb_scraped"]]) {
    final <- paste0(dir, sources$fb_scraped$final)
    if (!file.exists(final)) {
      if (verbose) message("reformatting Facebook Scraped dataset")
      original <- paste0(dir, sources$fb_scraped$original)
      raw <- arrow::read_csv_arrow(original)
      raw <- do.call(rbind, lapply(split(raw, raw$`Firstname&nicknames`), function(d) {
        names <- strsplit(tolower(d[[1]]), ",", fixed = TRUE)[[1]]
        total <- d$`# of times the name is labeled as female` + d$`# of times the name is labeled as male`
        data.frame(
          name = names,
          count = total,
          prob_fem = d$`# of times the name is labeled as female`,
          check.names = FALSE
        )
      }))
      raw <- do.call(rbind, lapply(split(raw, raw$name), function(d) {
        if (nrow(d) == 1) {
          d$prob_fem <- d$prob_fem / d$count
          d
        } else {
          total <- sum(d$count)
          data.frame(name = d[1, 1], count = total, prob_fem = sum(d$prob_fem) / total)
        }
      }))
      arrow::write_csv_arrow(raw, final)
      rm(raw)
    }
    initial <- as.data.frame(arrow::read_csv_arrow(final))
    found <- lgiven %in% initial$name
    if (any(found)) {
      initial <- initial[initial$name %in% lgiven, ]
      rownames(initial) <- initial$name
      mres <- initial[lgiven[found], -1]
      if (!all(found)) {
        nmissed <- sum(!found)
        mres <- rbind(
          mres, as.data.frame(matrix(
            rep(c(0, .5), each = nmissed), nmissed,
            dimnames = list(lgiven[!found], colnames(mres))
          ))
        )
      }
      colnames(mres) <- paste0(colnames(mres), "_fb_scraped")
      res <- cbind(res, mres[lgiven, ])
      rownames(res) <- NULL
    }
  }
  if (requested[["skydeck"]]) {
    final <- paste0(dir, sources$skydeck$final)
    if (!file.exists(final)) {
      if (verbose) message("reformatting skydeck dataset")
      original <- paste0(dir, sources$skydeck$original)
      raw <- arrow::read_csv_arrow(original)
      raw <- as.data.frame(do.call(rbind, lapply(split(raw, raw$Name), function(d) {
        total <- sum(d$Count)
        fem_count <- as.numeric(d[d$Gender == "F", "Count"])
        c(count = total, prob_fem = if (is.na(fem_count)) 0 else fem_count / total)
      })))
      raw <- cbind(name = rownames(raw), raw)
      arrow::write_csv_arrow(raw, final)
      rm(raw)
    }
    initial <- as.data.frame(arrow::read_csv_arrow(final))
    found <- given %in% initial$name
    if (any(found)) {
      initial <- initial[initial$name %in% given, ]
      rownames(initial) <- initial$name
      mres <- initial[given[found], -1]
      if (!all(found)) {
        nmissed <- sum(!found)
        mres <- rbind(
          mres, as.data.frame(matrix(
            rep(c(0, .5), each = nmissed), nmissed,
            dimnames = list(given[!found], colnames(mres))
          ))
        )
      }
      colnames(mres) <- paste0(colnames(mres), "_skydeck")
      res <- cbind(res, mres[given, ])
      rownames(res) <- NULL
    }
  }
  if (requested[["usssa"]]) {
    final <- paste0(dir, sources$usssa$final)
    if (!file.exists(final)) {
      if (verbose) message("reformatting US SSA dataset")
      raw <- get_baby_names(dir, ssa_source)
      raw <- as.data.frame(raw$summary)
      raw <- data.frame(
        name = rownames(raw),
        count = raw$count,
        prob_fem = raw$female / (raw$female + raw$male)
      )
      arrow::write_csv_arrow(raw, final)
      rm(raw)
    }
    initial <- as.data.frame(arrow::read_csv_arrow(final))
    found <- given %in% initial$name
    if (any(found)) {
      initial <- initial[initial$name %in% given, ]
      rownames(initial) <- initial$name
      mres <- initial[given[found], -1]
      if (!all(found)) {
        nmissed <- sum(!found)
        mres <- rbind(
          mres, as.data.frame(matrix(
            rep(c(0, .5), each = nmissed), nmissed,
            dimnames = list(given[!found], colnames(mres))
          ))
        )
      }
      colnames(mres) <- paste0(colnames(mres), "_usssa")
      res <- cbind(res, mres[given, ])
      rownames(res) <- NULL
    }
  }
  if (length(ogiven) != nrow(res)) {
    rownames(res) <- res$given
    res <- res[ogiven, ]
    rownames(res) <- NULL
  }
  if (!is.null(country)) {
    res$country <- country
    if (requested[["wgnd"]]) {
      res$sex_in_country_wgnd <- "U"
      matched_wgnd <- as.data.frame(matched_wgnd)
      if (anyNA(matched_wgnd$code) && "NA" %in% res$country) {
        matched_wgnd[is.na(matched_wgnd$code), "code"] <- "NA"
      }
      provided <- paste(tolower(res$given), res$country)
      matched_wgnd <- matched_wgnd[matched_wgnd$code %in% res$country, ]
      matched_wgnd$pair <- paste(matched_wgnd$name, matched_wgnd$code)
      matched_wgnd <- matched_wgnd[matched_wgnd$pair %in% provided, ]
      if (nrow(matched_wgnd)) {
        found <- provided %in% matched_wgnd$pair
        res[found, "sex_in_country_wgnd"] <- unname(structure(matched_wgnd$gender, names = matched_wgnd$pair)[provided[found]])
      }
    }
  }
  if (requested[["fb"]]) {
    if (!is.null(family)) {
      if (anyNA(family)) family[is.na(family)] <- ""
      is_lowercase <- !grepl("^[A-Z]", family, perl = TRUE)
      if (any(is_lowercase)) family[is_lowercase] <- gsub("\\b(\\w)", "\\U\\1", family[is_lowercase], perl = TRUE)
      family[is.na(family) | family == ""] <- " "
      res$family <- family
      ufamily <- unique(res$family)
      first_raw <- as.character(vapply(iconv(substring(ufamily, 1, 1), toRaw = TRUE), "[[", raw(1), 1))
      db <- arrow::open_dataset(paste0(dir, sources$fb$final[[2]]))
      initial <- dplyr::compute(dplyr::filter(db, start %in% first_raw))
      if (nrow(initial)) {
        mres <- as.data.frame(dplyr::compute(dplyr::filter(initial, name %in% ufamily)))
        if (nrow(mres)) {
          rownames(mres) <- mres$name
          found <- ufamily %in% mres$name
          mres <- mres[, !colnames(mres) %in% c("name", "start")]
          nmissed <- sum(!found)
          if (nmissed) {
            mres <- rbind(
              mres, as.data.frame(matrix(
                rep(numeric(ncol(mres)), each = nmissed), nmissed,
                dimnames = list(ufamily[!found], colnames(mres))
              ))
            )
          }
          colnames(mres) <- paste0(colnames(mres), "_fb")
          mres <- mres[res$family, ]
          if (all(colnames(mres) %in% colnames(res))) {
            sum <- !is.na(mres)
            mres[!sum] <- 0
            sres <- res[, colnames(mres)]
            ssum <- !is.na(sres)
            sres[!ssum] <- 0
            sum <- sum + ssum
            sum[sum == 0] <- 1
            res[, colnames(mres)] <- (sres + mres) / sum
          } else {
            res <- cbind(res, mres)
          }
        }
      }
    }
    if (!full_country) {
      cis <- grep("^[A-Z]{2}_fb$", colnames(res))
      if (length(cis)) {
        res <- cbind(res[, -cis], predicted_country = sub("_fb", "", colnames(res)[cis][max.col(res[, cis], "first")], fixed = TRUE))
      }
    }
  }
  res
}
