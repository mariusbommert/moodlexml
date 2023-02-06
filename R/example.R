#' @title Path to example files in extdata.
#'
#' @description Get the path to files in extdata. This is necessary for the
#' examples.
#'
#' @param name [\code{character}]\cr Name of example file. For NULL the names of
#' all available example files are listed.
#'
#' @return Path to example file(s).
#'
#' @examples read_example(name = "text.png")
#'
#' @export
read_example = function(name = NULL) {
  # check input:
  assert_character(x = name, any.missing = FALSE, null.ok = TRUE)

  if(is.null(name)) {
    dir(path = system.file("extdata", package = "moodlexml"))
  } else {
    tryCatch(system.file("extdata", name, package = "moodlexml", mustWork = TRUE),
             error = function(e) paste0("File ", name, " not found."))
  }
}
