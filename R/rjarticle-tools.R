unparsed <- function(...) {
    structure(list(...), class = c("unparsed", "article"))
}
make_article <- function(id, slug = "",
                         authors = "", title = "", editor = "", ae = "",
                         reviewers = "", status = "", path = "",type = "",
                         suppl = "", keywords = "", otherids = "") {
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

article <- function(..., quiet = FALSE) {
    tryCatch(make_article(...),
             error = function(e) {
                 article <- unparsed(...)
                 if (!quiet) {
                     message("Failed to parse: ")
                     message(article)
                     message(e, "\n")
                 }
                 article
             }
    )
}

load_article <- function(path, quiet = FALSE) {
    fields <- c("ID", "Slug", "Authors", "Keywords",
                "OtherIDs", "Title", "Editor", "AE", "Reviewers", "Status", "Suppl")
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

"%||%" <- function(a, b) if (empty(a)) b else a

"%NA%" <- function(a, b) ifelse(is.na(a), b, a)

"%@%" <- function(x, attribute) {
    attr(x, attribute)
}
