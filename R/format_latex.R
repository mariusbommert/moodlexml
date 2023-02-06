#' @title Format vector or matrix as LaTeX pmatrix.
#'
#' @description Format vector or matrix as LaTeX pmatrix.
#'
#' @param values [\code{numeric}]\cr Vector or matrix of numeric values.
#'
#' @return LaTeX Code for vector or matrix as pmatrix.
#'
#' @examples
#' pmatrix(values = c(1, 2))
#' pmatrix(values = diag(2))
#'
#' @export
pmatrix = function(values) {
  # check input:
  assert_numeric(x = values, any.missing = FALSE)

  # format vector or matrix to pmatrix:
  if(is.null(dim(values))) values = matrix(values, ncol = 1)
  rows = nrow(values)
  cols = ncol(values)
  pm = ""
  for(i in seq_len(rows)) {
    for(j in seq_len(cols)) {
      if(j == 1 & j == cols) {
        if(i != rows) {
          pm = paste0(pm, values[i, j], ' \\\\ ')
        } else {
          pm = paste0(pm, values[i, j], ' ')
        }
      } else if(j == 1) {
        pm = paste0(pm, ' ', values[i, j])
      } else if(j == cols) {
        if(i != rows) {
          pm = paste0(pm, ' & ', values[i, j], '\\\\')
        } else {
          pm = paste0(pm, ' & ', values[i, j], ' ')
        }
      } else {
        pm = paste0(pm, ' & ', values[i, j])
      }
    }
  }

  # return pmatrix:
  return(paste0('\\begin{pmatrix}', pm, '\\end{pmatrix}'))
}

#' @title Paste double-dollar around text.
#'
#' @description Paste double-dollar around text.
#'
#' @param text [\code{character}]\cr Text which can be coerced to type character.
#'
#' @return The text with double-dollar.
#'
#' @examples pd(text = 1:10)
#'
#' @export
pd = function(text) {
  # check input:
  if(!test_character(x = text)) {
    text = tryCatch(as.character(text), error = function(e) stop("invalid text"))
  }
  assert_character(x = text, any.missing = FALSE, min.len = 1)

  # return double-dollar pasted around text:
  return(purrr::map_chr(text, function(x) paste0('$$', x, '$$')))
}

#' @title Read LaTeX formulas from arbitrary file with 1 formula per row.
#'
#' @description Read LaTeX formulas from a file with 1 formula per row.
#'
#' @param file [\code{character}]\cr File name.
#' @param add_p [\code{flag}]\cr TRUE for adding a paragraph per row. Default is TRUE.
#' @param collapse [\code{flag}]\cr TRUE to collapse text. Default is FALSE.
#'
#' @return LaTeX formulas as character.
#'
#' @details At the moment only simple formulas can be read correctly. It may
#' be necessary to remove some special characters or other LaTeX code before
#' reading the file.
#'
#' @examples read_latex(file = read_example("text.txt"))
#'
#' @export
read_latex = function(file, add_p = TRUE, collapse = FALSE) {
  # check input:
  assert_character(x = file, any.missing = FALSE)
  assert_flag(x = add_p)
  assert_flag(x = collapse)

  if(file.exists(file)) {
    # read lines:
    x = readLines(file)

    # replace $ with $$:
    x = stringr::str_replace_all(string = x, pattern = stringr::fixed("$"), replacement = "$$")

    # remove empty lines:
    x = x[!(x == "")]

    # add paragraph and remove whitespace:
    add_paragraph = function(text) {
      return(p(stringr::str_trim(text)))
    }
    if(add_p) x = purrr::map(x, add_paragraph)

    # collapse text:
    if(collapse) x = paste0(x, collapse = "")

    return(x)
  } else {
    warning(paste0('File ', file, ' does not exist.'))
  }
}

#' @title Write a matrix to LaTeX.
#'
#' @description Write a matrix to LaTeX.
#'
#' @param m [\code{matrix|vector}]\cr matrix or vector to write to LaTeX.
#' @param cname [\code{character}]\cr column name for the matrix. Default is NULL
#' for no column name.
#' @param rname [\code{character}]\cr row name for the matrix. Default is NULL
#' for no row name.
#' @param name [\code{character}]\cr Filename for written file. Default is
#' "text.txt".
#' @param force [\code{flag}]\cr Flag for forcing writing of file. Default is
#' FALSE.
#' @param append [\code{flag}]\cr Flag for appending matrix to file. Default is
#' FALSE.
#' @param write_file [\code{flag}]\cr Flag for writing file. Default is TRUE.
#' Otherwise the matrix is returned.
#'
#' @return Write matrix as LaTeX.
#'
#' @details Write a matrix as LaTeX. Further options for formatting the matrix
#' would be beneficial.
#'
#' @examples
#' \dontrun{
#'   write_matrix_latex(m = diag(2), cname = c("A", "B"), rname = c("A", "B"))
#' }
#'
#' @export
write_matrix_latex = function(m, cname = NULL, rname = NULL, name = "text.txt",
                              force = FALSE, append = FALSE, write_file = TRUE) {
  # check input:
  assert_true(check_matrix(x = m) || check_vector(x = m))
  if(is.null(dim(m))) m = matrix(m, nrow = 1)
  assert_character(x = cname, min.len = ncol(m), max.len = ncol(m) + 1,
                   null.ok = TRUE)
  assert_character(x = rname, len = nrow(m), null.ok = TRUE)
  assert_character(x = name, min.chars = 3, any.missing = FALSE, len = 1)
  assert_flag(x = force)
  assert_flag(x = append)
  assert_flag(x = write_file)

  # function for rows:
  row = function(x) paste0(x, collapse = " & ")

  # build table:
  if(!file.exists(name) || force) {
    if(!is.null(rname)) {
      text = paste0('\\begin{tabular}{', paste0(rep("c", ncol(m) + 1), collapse = ""), '}')
    } else {
      text = paste0('\\begin{tabular}{', paste0(rep("c", ncol(m)), collapse = ""), '}')
    }
    if(!is.null(cname)) {
      if(length(cname) == ncol(m) & !is.null(rname)) {
        text = c(text, paste0('& ', paste0(row(cname), '\\\\')))
      } else {
        text = c(text, paste0(row(cname), '\\\\'))
      }
    }
    if(!is.null(rname)) {
      text = c(text, vapply(seq_len(nrow(m)), function(x)
        paste0(row(c(rname[x], m[x, ])), "\\\\"), character(1)))
    } else {
      text = c(text, vapply(seq_len(nrow(m)), function(x)
        paste0(row(m[x, ]), "\\\\"), character(1)))
    }
    text = c(text, '\\end{tabular}')
    if(write_file) {
      write(x = text, file = name, append = append)
    } else {
      return(text)
    }
  } else {
    warning("File ", name, " already exists. Use force = TRUE to force writing.")
  }
}
