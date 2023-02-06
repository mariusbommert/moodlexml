#' @title Show text as html.
#'
#' @description Show text as html in browser.
#'
#' @param text [\code{character}]\cr Text which should be rendered as html.
#'
#' @return Nothing returned but html preview opened.
#'
#' @details This preview is only able to display basic html elements. The
#' possibility to display more types of Moodle objects would be appreciated.
#'
#' @examples
#' \dontrun{
#' show_html(text = "text")
#' }
#'
#' @export
show_html = function(text) {
  # check input:
  assert_character(x = text, any.missing = FALSE, len = 1)

  # check if package htmltools is available:
  if(!requireNamespace("htmltools", quietly = TRUE)) {
    warning(paste0('Install package htmltools to use this function.'))
  }
  # check if package httr is available:
  if(!requireNamespace("httr", quietly = TRUE)) {
    warning(paste0('Install package httr to use this function.'))
  }

  # show text as html:
  tmp = paste0('<!DOCTYPE html><html><head></head><body>',
               htmltools::HTML(text = text),
               '</body>')
  temp_file = tempfile(fileext = ".html")
  htmltools::save_html(html = htmltools::renderDocument(htmltools::htmlTemplate(text_ = tmp)),
                       file = temp_file)

  # browse html in default browser:
  httr::BROWSE(temp_file)
}

