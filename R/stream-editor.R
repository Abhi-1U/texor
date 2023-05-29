
#' @title stream editor
#' @description R equivalent of GNU-sed
#'
#' @param raw_lines a vector of readLines from the file
#' @param pattern a regex pattern to match
#' @param target target string to be replaced
#' @param replacement replacement string to be substituted
#'
#' @return raw_lines : modified  vector of lines
#' @export
#' @examples
#' example_string <- "\\target{} \\not_a_target{}"
#' texor::stream_editor(example_string,"\\s*\\\\target\\{\\}", "\\\\target", "\\\\hit")
stream_editor <- function(raw_lines, pattern, target, replacement) {
    break_points <- which(grepl(pattern, raw_lines))
    #check for length of break_points to avoid no matches
    if (!identical(break_points, integer(0))) {
        for (iterator in seq_along(break_points)) {
            modification_line <- raw_lines[break_points[iterator]]
            modified_line <- gsub(target, replacement, modification_line)
            raw_lines[break_points[iterator]] <- modified_line
        }
        message(paste(
            "Found ", length(break_points), "Matches for target :", target))
    } else {
        message(paste("Found 0 Matches for target : ", target))
    }
    return(raw_lines)
}
