unparsed <- function(...) {
    structure(list(...), class = c("unparsed", "article"))
}
make_article <- function(id, slug = "", authors = "", title = "", editor = "", ae = "",
                         reviewers = "", status = "", path = "",type = "", suppl = "",
                         keywords = "", otherids = "") {
    structure(list(
        id = parse_id(id),
        other_id = otherids,
        slug = slug,
        suppl = parse_supplementaries(suppl),
        path = path,
        type = type,
        authors = parse_address_list(authors),
        keywords = str_c(keywords, sep = ", "),
        title = str_trim(title),
        editor = str_trim(editor),
        ae = str_trim(ae),
        reviewers = parse_address_list(reviewers),
        status = parse_status_list(status)
    ), class = "article")
}
parse_supplementaries <- function(suppl) {
  x <- str_trim(str_split(suppl, ",\\s*\n")[[1]])
  x <- x[str_length(x) > 0]
  xs <- lapply(x, function(y) {
    class(y) <- "supplfile"
    y
  })
  class(xs) <- "supplfile_list"
  xs
}
Parse_status_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  x <- trimws(x)
  if (empty(x)) {
    return(status_list())
  }

  statuses <- trimws(strsplit(x, ",[ \t\r]*(\n|$)")[[1]])
  statuses <- statuses[statuses != ""]

  status_list(lapply(statuses, parse_status))
}

parse_status <- function(x) {
  x <- stringr::str_trim(x)

  re <- "^(\\d{4}-\\d{2}-\\d{2}) ([^\\[]*)(?: \\[([^\\[]+)\\])?$"
  if (!stringr::str_detect(x, re)) {
    # NM added line
    cat("bad status:", x, "\n")
    stop("Status must have form 'yyyy-mm-dd status [optional comments]'",
      call. = FALSE
    )
  }

  pieces <- stringr::str_match(x, re)[1, ]

  date <- pieces[2]
  if (!is.date(date)) stop("Date must be a valid date")
  date <- as.Date(date)

  status <- pieces[3]
  comments <- if (is.na(pieces[4])) "" else pieces[4]

  status(status = status, date = date, comments = comments)
}
article <- function(..., quiet = FALSE) {
  tryCatch(make_article(...),
    error = function(e) {
      article <- unparsed(...)
      if (!quiet) {
        message("Failed to parse: ")
        print(article)
        message(e, "\n")
      }
      article
    }
  )
}

load_article <- function(path, quiet = FALSE) {
  fields <- c("ID", "Slug", "Authors", "Keywords", "OtherIDs", "Title", "Editor", "AE", "Reviewers", "Status", "Suppl")
  dcf <- read.dcf(path, fields = fields, keep.white = fields)
  if (nrow(dcf) != 1) stop("DCF parsing error: ", path, call. = FALSE)

  # Remove field names that keep.white incorrectly preserves
  for (field in fields) {
    dcf[, field] <- gsub(paste(field, ": ?", sep = ""), "", dcf[, field])
  }
  # Convert missing values to empty strings
  dcf[is.na(dcf)] <- ""
  colnames(dcf) <- tolower(colnames(dcf))

  dcf <- as.list(as.data.frame(dcf, stringsAsFactors = FALSE))
  # Only should be manually set in tests
  if (is.null(dcf$id) || identical(dcf$id, "")) {
    dcf$id <- basename(dirname(path))
  }
  dcf$path <- dirname(path)
  do.call(article, dcf)
}
parse_id <- function(x) {
  if (is.id(x)) {
    return(x)
  }

  re <- "^([0-9]{4})-([0-9]{2,3})[a-z]?$"

  x <- str_trim(x)

  if (!str_detect(x, re)) stop("ID must have form XXXX-YYY?")

  pieces <- str_match(x, re)[1, ]
  year <- pieces[2]
  seq <- pieces[3]

  if (!is.number(year)) stop("Year must be a number")
  if (!is.number(seq)) stop("ID must be a number")

  if (year > year()) stop("Year must be in the present or past")
  if (year < 2002) stop("Year must be >= 2002")

  id(as.integer(year), as.integer(seq))
}

id <- function(year, seq) {
  stopifnot(is.numeric(year), length(year) == 1)
  stopifnot(is.numeric(seq), length(seq) == 1)

  year <- as.integer(year)
  seq <- as.integer(seq)

  structure(list(year = year, seq = seq), class = "id")
}

is.id <- function(x) inherits(x, "id")

#' @method format id
#' @export
format.id <- function(x, ...) {
  paste(x$year, sprintf("%02d", x$seq), sep = "-")
}
#' @method print id
#' @export
print.id <- function(x, ...) cat(format(x), "\n")

year <- function() as.POSIXlt(Sys.Date())$year + 1900

is.number <- function(x) {
  suppressWarnings(!is.na(as.numeric(x)))
}

#' @method as.character id
#' @export
as.character.id <- function(x, ...)
  format.id(x)

parse_address_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  if (empty(x)) {
    return(address_list())
  }

  addresses <- str_trim(str_split(x, ",")[[1]])
  address_list(lapply(addresses, parse_address))
}

address_list <- function(addresses = list()) {
  stopifnot(is.list(addresses))
  structure(addresses, class = "address_list")
}

address <- function(email = NULL, name = NULL, comment = NULL) {
  if (is.null(email) && is.null(name)) {
    stop("Address must have name or email", call. = FALSE)
  }

  structure(list(name = name, email = email, comment = comment), class = "address")
}

#' @export
format.address <- function(x, ...) {
  name <- if (!is.null(x$name)) paste('"', x$name, '"', sep = "")
  email <- if (!is.null(x$email)) paste("<", x$email, ">", sep = "")
  comment <- if (!is.null(x$comment)) paste("[", x$comment, "]", sep = "")

  paste(c(name, email, comment), collapse = " ")
}
parse_address <- function(x) {
  stopifnot(is.character(x), length(x) == 1)

  pieces <- str_match(x, "^\\s*([^<>]*) ?(<.*>)? ?(\\[.*\\])?")[1, ]

  comment <- str_trim(pieces[4])
  comment <- str_replace_all(comment, "\\[|\\]", "")
  if (is.na(comment) || str_length(comment) == 0) comment <- NULL

  email <- str_trim(pieces[3])
  email <- str_replace_all(email, "<|>", "")
  if (is.na(email) || str_length(email) == 0) email <- NULL

  name <- str_trim(pieces[2])
  name <- str_replace_all(name, fixed('"'), "")
  if (is.na(name) || str_length(name) == 0) name <- NULL

  address(email, name, comment)
}
parse_status_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  x <- trimws(x)
  if (empty(x)) {
    return(status_list())
  }

  statuses <- trimws(strsplit(x, ",[ \t\r]*(\n|$)")[[1]])
  statuses <- statuses[statuses != ""]

  status_list(lapply(statuses, parse_status))
}

parse_status <- function(x) {
  x <- stringr::str_trim(x)

  re <- "^(\\d{4}-\\d{2}-\\d{2}) ([^\\[]*)(?: \\[([^\\[]+)\\])?$"
  if (!stringr::str_detect(x, re)) {
    # NM added line
    cat("bad status:", x, "\n")
    stop("Status must have form 'yyyy-mm-dd status [optional comments]'",
      call. = FALSE
    )
  }

  pieces <- stringr::str_match(x, re)[1, ]

  date <- pieces[2]
  if (!is.date(date)) stop("Date must be a valid date")
  date <- as.Date(date)

  status <- pieces[3]
  comments <- if (is.na(pieces[4])) "" else pieces[4]

  status(status = status, date = date, comments = comments)
}
# status_list class ------------------------------------------------------------

status_list <- function(x = list()) {
  structure(x, class = "status_list")
}

format.status_list <- function(x, ...) {
  statuses <- lapply(x, format)
  paste(statuses, collapse = ",\n  ")
}

print.status_list <- function(x, ...) {
  statuses <- lapply(x, format)
  cat(paste(statuses, collapse = "\n"))
}
is.status_list <- function(x) inherits(x, "status_list")
final_status <- c(
    "reject and resubmit",
    "published",
    "withdrawn",
    "rejected"
)

valid_status <- c(
  "submitted",
  "acknowledged",
  "passed initial checks",
  "needs reviewers",
  "needs editor",
  "updated",
  "out for review",
  "major revision",
  "minor revision",
  "revision received",
  "accepted",
  "copy edited",
  "online",
  "proofed",
  "out for proofing",
  "style checked",
  "with AE",
  "AE: major revision",
  "AE: minor revision",
  "AE: accept",
  "AE: reject",
  final_status
)

status <- function(status, date = Sys.Date(), comments = "") {
  stopifnot(is.Date(date), length(date) == 1)
  stopifnot(is.character(status), length(status) == 1)
  stopifnot(is.character(comments), length(comments) == 1)

  # Date + 1 provides a buffer for timezones with remote resources.
  if (date > (Sys.Date() + 1)) stop("Date must not be in the future")
  if (date < as.Date("2002-01-01")) {
    stop("Date must not before the R journal was created")
  }

  status <- str_trim(status)
  if (!(status %in% valid_status)) {
    guess <- amatch_status(status)
    if (tolower(status) == tolower(guess)) {
      status <- guess
    } else {
      stop(status, " is not a known status. ",
        "Did you mean ", amatch_status(status), "?",
        call. = FALSE
      )
    }
  }

  structure(list(date = date, status = status, comments = comments),
    class = "status"
  )
}

is.status <- function(x) inherits(x, "status")


c.status <- c.status_list <- function(..., recursive = FALSE) {
  pieces <- list(...)
  statuses <- lapply(pieces, function(x) {
    if (is.status(x)) {
      list(x)
    } else if (is.status_list(x)) {
      x
    } else {
      stop("Don't know how to combine with ", class(x)[1])
    }
  })

  status_list(unlist(statuses, recursive = FALSE))
}

#' @export
format.status <- function(x, ...) {
  paste(format(x$date), " ", x$status,
    if (!empty(x$comments)) paste(" [", x$comments, "]", sep = ""),
    sep = ""
  )
}
#' @export
print.status <- function(x, ...) cat(format(x), "\n")

#' @importFrom utils adist
amatch_status <- function(status) {
  ldist <- adist(status, valid_status,
    ignore.case = TRUE, partial = FALSE,
    costs = c(ins = 0.5, sub = 1, del = 2)
  )[1, ]
  valid_status[which.min(ldist)]
}

is.date <- function(x) {
  parsed <- strptime(x, "%Y-%m-%d")
  !is.na(parsed) && format(parsed) == x
}

is.Date <- function(x) inherits(x, "Date")


format_non_null <- function(x) if(is.null(x)) NULL else format(x)