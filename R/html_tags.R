#' @title Tagged text.
#'
#' @description Add a html-tag to the given text.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#' @param tag [\code{character}]\cr Tag name.
#'
#' @return Tagged text.
#'
#' @examples tag(text = "text", tag = "p")
#'
#' @export
tag = function(text, tag) {
  # check input:
  if(!test_character(x = text)) {
    text = tryCatch(as.character(text), error = function(e) stop("invalid text"))
  }
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_character(x = tag, any.missing = FALSE, len = 1)

  # return tagged text:
  return(paste0('<', tag, '>', text, '</', tag, '>'))
}

#' @title Left aligned paragraph of text.
#'
#' @description Left align the text in a paragraph.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#'
#' @return Left aligned text.
#'
#' @examples p_left(text = "text")
#'
#' @export
p_left = function(text) {
  # check input:
  if(!test_character(x = text)) {
    text = tryCatch(as.character(text), error = function(e) stop("invalid text"))
  }
  assert_character(x = text, any.missing = FALSE, len = 1)

  # return left aligned text:
  return(paste0('<p dir="ltr" style="text-align: left;">', text, '</p>'))
}

#' @title Paragraph of text.
#'
#' @description Put text in paragraph.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#'
#' @return The text in a paragraph.
#'
#' @examples p(text = "text")
#'
#' @export
p = function(text) {
  # return text in paragraph:
  return(tag(text = text, tag = "p"))
}

#' @title Bold text.
#'
#' @description Text in bold.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#'
#' @return Text in bold.
#'
#' @examples b(text = "text")
#'
#' @export
b = function(text) {
  # return bold text:
  return(tag(text = text, tag = "b"))
}

#' @title Italic text.
#'
#' @description Text in italic.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#'
#' @return Text in italic.
#'
#' @examples i(text = "text")
#'
#' @export
i = function(text) {
  # return italic text:
  return(tag(text = text, tag = "i"))
}

#' @title Line break after text.
#'
#' @description Text with line break.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#'
#' @return Text with line break.
#'
#' @examples br(text = "text")
#'
#' @export
br = function(text) {
  # check input:
  if(!test_character(x = text)) {
    text = tryCatch(as.character(text), error = function(e) stop("invalid text"))
  }
  assert_character(x = text, any.missing = FALSE, len = 1)

  # return line break after text:
  paste(text, '<br>')
}

#' @title Text as list item.
#'
#' @description Text as list item.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#'
#' @return Text as list item.
#'
#' @examples li(text = "text")
#'
#' @export
li = function(text) {
  # return text as list item:
  return(tag(text = text, tag = "li"))
}

#' @title Colored text.
#'
#' @description Colored text.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#' @param color [\code{choice}]\cr Name of colors available with grDevices::colors().
#' Default color is "red".
#'
#' @return Colored text.
#'
#' @examples col_rgb(text = "text")
#'
#' @export
col_rgb = function(text, color = "red") {
  # check if package grDevices is available:
  if(!requireNamespace("grDevices", quietly = TRUE)) {
    warning(paste0('Install package grDevices to use this function.'))
  }

  # check input:
  if(!test_character(x = text)) {
    text = tryCatch(as.character(text), error = function(e) stop("invalid text"))
  }
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_choice(x = color, choices = grDevices::colors(), null.ok = FALSE)

  # return colored text:
  col = grDevices::col2rgb(color)
  return(paste0('<span class="" style="color: rgb(', paste0(col[, 1], collapse = ", "),
                ');">', text, '</span>'))
}

#' @title Ordered or unordered list.
#'
#' @description Ordered or unordered list.
#'
#' @param items [\code{character}]\cr Items which can be coerced to type character.
#' @param ordered [\code{flag}]\cr TRUE for ordered list. FALSE for unordered list.
#' Default is FALSE.
#'
#' @return Ordered or unordered list.
#'
#' @examples oul(items = 1:5)
#'
#' @export
oul = function(items, ordered = FALSE) {
  # check input:
  if(!test_character(x = items)) {
    items = tryCatch(as.character(items), error = function(e) stop("invalid text"))
  }
  assert_character(x = items, any.missing = FALSE, min.len = 1)

  # return ordered or unordered list:
  tmp = purrr::map(items, li)
  return(paste0('<',ifelse(ordered, 'o', 'u'), 'l>', paste0(tmp, collapse = ""),
                '</', ifelse(ordered, 'o', 'u'), 'l>'))
}

#' @title Link a file.
#'
#' @description Get a link to a file.
#'
#' @param name [\code{character}]\cr Name of file which should be linked.
#' @param text [\code{character}]\cr Text which is used as label for the link.
#'
#' @return A link to a file.
#'
#' @examples
#' link_file(name = read_example("text.txt"), text = "text.txt")
#'
#' @export
link_file = function(name, text) {
  # check input:
  assert_character(x = name, any.missing = FALSE, len = 1)
  if(!test_character(x = text)) {
    text = tryCatch(as.character(text), error = function(e) stop("invalid text"))
  }
  assert_character(x = text, any.missing = FALSE, len = 1)

  # link to the file:
  if(file.exists(name)) {
    return(paste0('<a href="@@PLUGINFILE@@/', basename(name), '">', text, '</a>'))
  } else {
    warning(paste0('File ', name, ' does not exist.'))
  }
}
