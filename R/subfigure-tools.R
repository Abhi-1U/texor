#' Patch subfigure environments in a LaTeX file
#'
#' Converts `subfigure` environments contained in LaTeX figures into
#' `tabular` layouts using the Lua converter shipped with texor.
#'
#' @param file Path to the LaTeX file to modify.
#' @param output_file Path to the output LaTeX file. Defaults to `file`,
#'   so the input file is modified in place.
#' @param ref_commands Character vector of LaTeX reference commands to
#'   rewrite. Defaults to `"ref"`.
#' @param backup Logical; if `TRUE` and `output_file` is the same as
#'   `file`, create `file.bk` before modifying the file.
#'
#' @return Invisibly returns the path to the converted LaTeX file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tex_file <- "article.tex"
#' patch_subfigure_env(tex_file)
#'
#' # Or write to a separate file
#' patch_subfigure_env(
#'   tex_file,
#'   output_file = "article-converted.tex"
#' )
#'
#' # Rewrite additional cross-reference commands
#' patch_subfigure_env(
#'   tex_file,
#'   ref_commands = c("ref", "autoref", "cref", "Cref")
#' )
#' }
patch_subfigure_env <- function(file,
                                output_file = file,
                                ref_commands = "ref",
                                backup = TRUE) {

    if (!is.character(file) || length(file) != 1L) {
        stop("`file` must be a single file path.", call. = FALSE)
    }

    if (!file.exists(file)) {
        stop("LaTeX file does not exist: ", file, call. = FALSE)
    }

    if (!is.character(output_file) || length(output_file) != 1L) {
        stop("`output_file` must be a single file path.", call. = FALSE)
    }

    if (!is.character(ref_commands) || length(ref_commands) == 0L) {
        stop("`ref_commands` must contain at least one command.", call. = FALSE)
    }

    # Normalize paths so comparisons and Pandoc invocation are predictable.
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
    output_file <- normalizePath(
        output_file,
        winslash = "/",
        mustWork = FALSE
    )

    runner <- system.file(
        "subfigure_runner.lua",
        package = "texor"
    )

    if (runner == "") {
        stop(
            "Could not find texor's subfigure Lua filter.",
            call. = FALSE
        )
    }

    # texor already requires Pandoc >= 3.1.
    if (!rmarkdown::pandoc_available("3.1")) {
        stop(
            "texor requires Pandoc version 3.1 or newer.",
            call. = FALSE
        )
    }

    # Always write to a temporary file first. This prevents a failed Lua
    # conversion from destroying the original LaTeX file.
    converted_file <- tempfile(
        pattern = "texor-subfigure-",
        fileext = ".tex"
    )

    pandoc_output <- tempfile(
        pattern = "texor-pandoc-",
        fileext = ".txt"
    )

    on.exit(
        unlink(c(converted_file, pandoc_output), force = TRUE),
        add = TRUE
    )

    # Preserve environment variables in case texor is being run inside
    # another conversion process.
    old_output <- Sys.getenv(
        "TEXOR_SUBFIGURE_OUTPUT",
        unset = NA_character_
    )

    old_refs <- Sys.getenv(
        "TEXOR_SUBFIGURE_REF_COMMANDS",
        unset = NA_character_
    )

    on.exit({
        if (is.na(old_output)) {
            Sys.unsetenv("TEXOR_SUBFIGURE_OUTPUT")
        } else {
            Sys.setenv(TEXOR_SUBFIGURE_OUTPUT = old_output)
        }

        if (is.na(old_refs)) {
            Sys.unsetenv("TEXOR_SUBFIGURE_REF_COMMANDS")
        } else {
            Sys.setenv(TEXOR_SUBFIGURE_REF_COMMANDS = old_refs)
        }
    }, add = TRUE)

    Sys.setenv(
        TEXOR_SUBFIGURE_OUTPUT = converted_file,
        TEXOR_SUBFIGURE_REF_COMMANDS = paste(ref_commands, collapse = ",")
    )

    # Pandoc is used here as the embedded Lua host. The actual LaTeX
    # transformation happens in subfigure_to_tabular.lua.
    rmarkdown::pandoc_convert(
        input = file,
        from = "latex",
        to = "plain",
        output = pandoc_output,
        options = rmarkdown::pandoc_lua_filter_args(runner),
        wd = dirname(file)
    )

    if (!file.exists(converted_file)) {
        stop(
            "The subfigure conversion did not produce an output file.",
            call. = FALSE
        )
    }

    # Determine whether this is an in-place modification.
    same_file <- identical(
        normalizePath(file, winslash = "/", mustWork = TRUE),
        normalizePath(output_file, winslash = "/", mustWork = FALSE)
    )

    if (same_file && backup) {
        backup_file <- paste0(file, ".bk")

        if (!file.copy(file, backup_file, overwrite = TRUE)) {
            stop(
                "Could not create backup file: ",
                backup_file,
                call. = FALSE
            )
        }
    }

    # Create output directory if necessary.
    output_dir <- dirname(output_file)

    if (!dir.exists(output_dir)) {
        dir.create(
            output_dir,
            recursive = TRUE,
            showWarnings = FALSE
        )
    }

    if (!file.copy(
        converted_file,
        output_file,
        overwrite = TRUE
    )) {
        stop(
            "Could not write converted LaTeX file: ",
            output_file,
            call. = FALSE
        )
    }

    invisible(output_file)
}
