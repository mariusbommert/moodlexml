# helper for adding partial points:
partial_ij = function(partial_answer, i, j, partial_point, partial_accept) {
  if(!is.null(partial_answer) && !is.null(partial_point)) {
    if(is.null(partial_accept[i, j]) || partial_accept[i, j] == 0) {
      return(paste0('~%', partial_point[i, j], '%', partial_answer[i, j]))
    } else {
      return(paste0('~%', partial_point[i, j], '%', partial_answer[i, j], ':', partial_accept[i, j]))
    }
  }
}

# helper for formatting cells:
format_cell = function(m, i, j, is_graded, point, accepted_error,
                       wrong_default, type, partial_answer = NULL, partial_point = NULL,
                       partial_accept = NULL) {
  c_string = ''
  if(i <= nrow(m) & j <= ncol(m)) {
    if(is_graded) {
      if(is.null(accepted_error) || accepted_error == 0) {
        c_string = paste0('{', point[i, j], ':', type[i, j], ':=', m[i, j])
      } else {
        c_string = paste0('{', point[i, j], ':', type[i, j], ':=', m[i, j], ':', accepted_error)
      }
      if(!is.null(partial_answer) & !is.null(partial_point)) {
        if(is.list(partial_answer) & is.list(partial_point)) {
          for(k in seq_along(partial_point)) {
            c_string = paste0(c_string, partial_ij(partial_answer[[k]], i, j,
                                                   partial_point[[k]],
                                                   partial_accept[[k]]))
          }
        } else {
          c_string = paste0(c_string, partial_ij(partial_answer, i, j,
                                                 partial_point,
                                                 partial_accept))
        }
      }
      c_string = paste0(c_string, '~%0%', wrong_default, '}')
    } else {
      c_string = m[i, j]
    }
  } else {
    if(is_graded) {
      c_string = paste0('{', point[i, j], ':SA:=-~%0%', wrong_default, '}')
    } else {
      c_string = '-'
    }
  }
  c_string = paste0('<td>', c_string, '</td>')
  return(c_string)
}

#' @title Parse matrix as table.
#'
#' @description Parse a matrix as a string.
#'
#' @param m [\code{matrix}]\cr Matrix which should be parsed.
#' @param rows [\code{count}]\cr Number of rows for output table. Default is NULL
#' for same number of rows as in m.
#' @param cols [\code{count}]\cr Number of columns for output table. Default is NULL
#' for same number of columns as in m.
#' @param cname [\code{character}]\cr Column names.
#' @param rname [\code{character}]\cr Row names.
#' @param is_graded [\code{flag}]\cr TRUE for grading. FALSE for no grading.
#' @param accepted_error [\code{number}]\cr Accepted error. Default is NULL for
#' no accepted error.
#' @param wrong_default [\code{count}]\cr Length of wrong default answer. Default
#' is 7.
#' @param point [\code{number}]\cr Point for correct answer. Default is 1.
#' @param type [\code{choice}]\cr Types NM, SA or SAC.
#' @param partial_answer [\code{character}]\cr Partially correct answer.
#' @param partial_point [\code{number}]\cr Point for partially correct answer.
#' Default is NULL for no partial point.
#' @param partial_accept [\code{number}]\cr Accepted error for partially correct
#' answer. Default is NULL for no accepted error for partial answer.
#' @param caption [\code{character}]\cr Table caption. Default is "" for no caption.
#' @param width [\code{number}]\cr Width of table in percent. Default is 100.
#'
#' @return Matrix as character.
#'
#' @examples
#' parse_table(m = diag(1))
#'
#' @export
parse_table = function(m, rows = NULL, cols = NULL, cname = NULL, rname = NULL,
                       is_graded = TRUE, accepted_error = 0,
                       wrong_default = 7, point = NULL, type = "NM",
                       partial_answer = NULL, partial_point = NULL,
                       partial_accept = NULL, caption = "", width = 100){
  # check input:
  assert_count(x = rows, na.ok = FALSE, positive = TRUE, null.ok = TRUE)
  if(is.null(rows)) rows = nrow(m)
  assert_count(x = cols, na.ok = FALSE, positive = TRUE, null.ok = TRUE)
  if(is.null(cols)) cols = ncol(m)
  assert_matrix(x = m, max.rows = rows, null.ok = FALSE)
  matrix_rows = nrow(m)
  matrix_cols = ncol(m)
  if(!test_character(x = rname)) {
    if(!is.null(rname)) {
      rname = tryCatch(as.character(rname), error = function(e) stop("invalid rname"))
    }
  }
  assert_character(x = rname, any.missing = FALSE, max.len = rows, null.ok = TRUE)
  if(!test_character(x = cname)) {
    if(!is.null(cname)) {
      cname = tryCatch(as.character(cname), error = function(e) stop("invalid cname"))
    }
  }
  assert_character(x = cname, max.len = cols + 1, null.ok = TRUE)
  if(!is.null(rname)) {
    if(!is.null(cname)) {
      if(length(cname) == cols) {
        cname = c("", cname)
      } else if (length(cname) < cols) {
        cname = c(cname, rep("", cols - length(cname)))
      }
    }
    if(length(rname) < rows) {
      rname = c(rname, rep("", rows - length(rname)))
    }
  }
  tf = test_flag(x = is_graded)
  if(tf) is_graded = matrix(data = is_graded, nrow = rows, ncol = cols)
  assert_matrix(x = is_graded, nrows = rows, ncols = cols, any.missing = FALSE)
  assert_number(x = accepted_error, null.ok = FALSE)
  assert_count(x = wrong_default, positive = TRUE)
  wrong_default = as.numeric(paste0(1:wrong_default, collapse = ""))
  if(is.null(point)){
    point = matrix(data = 1, nrow = rows, ncol = cols)
  } else {
    if(length(point) == 1) {
      assert_number(x = point, lower = 0)
      point = matrix(data = point, nrow = rows, ncol = cols)
    } else {
      assert_matrix(x = point, nrows = rows, ncols = cols, null.ok = TRUE)
    }
  }
  if(length(type) == 1) {
    assert_choice(x = type, choices = c("NM", "SA", "SAC"))
    type = matrix(type, nrow = matrix_rows, ncol = matrix_cols)
  } else{
    assert_matrix(x = type, nrows = matrix_rows, ncols = matrix_cols)
  }

  if((is.null(partial_answer) && !is.null(partial_point)) ||
     (!is.null(partial_answer) && is.null(partial_point))) {
    warning("partial answer ignored")
  }

  if(test_number(partial_point, lower = 0, upper = 100)) {
    partial_point = matrix(partial_point, nrow = matrix_rows, ncol = matrix_cols)
  }
  if(test_number(partial_accept, lower = 0, finite = TRUE)) {
    partial_accept = matrix(partial_accept, nrow = matrix_rows, ncol = matrix_cols)
  }

  if(test_matrix(x = partial_answer, nrows = matrix_rows, ncols = matrix_cols)) {
    assert_matrix(x = partial_point, nrows = matrix_rows, ncols = matrix_cols)
    assert_numeric(x = partial_point, lower = 0, upper = 100, any.missing = FALSE)
    assert_matrix(x = partial_accept, nrows = matrix_rows, ncols = matrix_cols, null.ok = TRUE)
  }
  if(test_list(x = partial_answer, any.missing = FALSE)) {
    assert_list(x = partial_point, len = length(partial_answer))
    assert_list(x = partial_accept, len = length(partial_answer), null.ok = TRUE)
    for(i in seq_along(partial_answer)) {
      assert_matrix(x = partial_answer[[i]], nrows = matrix_rows, ncols = matrix_cols)
      assert_matrix(x = partial_point[[i]], nrows = matrix_rows, ncols = matrix_cols)
      assert_numeric(x = partial_point[[i]], lower = 0, upper = 100, any.missing = FALSE)
      assert_matrix(x = partial_accept[[i]], nrows = matrix_rows, ncols = matrix_cols, null.ok = TRUE)
    }
  }
  assert_character(x = caption, any.missing = FALSE)
  assert_number(x = width, lower = 0, upper = 100)

  # convert names to Moodle format:
  if(!is.null(cname)) {
    cnames = character(length(cname))
    for(j in seq_along(cnames)) {
      cnames[j] = paste0('<td>', cname[j], '</td>')
    }
  }
  if(!is.null(rname)) {
    rnames = character(length(rname))
    for(i in seq_along(rnames)) {
      rnames[i] = paste0('<td>', rname[i], '</td>')
    }
  }

  # convert cells to Moodle format:
  cells = matrix(nrow = rows, ncol = cols)
  for(i in 1:rows) {
    for(j in 1:cols) {
      cells[i, j] = format_cell(m = m, i = i, j = j, is_graded = is_graded[i, j],
                                point = point, accepted_error = accepted_error,
                                wrong_default = wrong_default, type = type,
                                partial_answer = partial_answer,
                                partial_point = partial_point,
                                partial_accept = partial_accept)
    }
  }

  # combine names and cells into single matrix:
  tmp = cells
  if(!is.null(rname)) tmp = cbind(rnames, cells)
  if(!is.null(cname)) tmp = rbind(cnames, tmp)

  # convert rows of table into Moodle format:
  table_str = ""
  for(i in 1:nrow(tmp)) {
    if(i == 1 & !is.null(cname)) {
      table_str = paste0(table_str, '<tr>', '<thead>')
      for(j in 1:ncol(tmp)) {
        table_str = paste0(table_str, tmp[i, j])
      }
      table_str = paste0(table_str, '</thead>', '</tr>')
    } else {
      table_str = paste0(table_str, '<tr>', '<tbody>')
      for(j in 1:ncol(tmp)) {
        table_str = paste0(table_str, tmp[i, j])
      }
      table_str = paste0(table_str, '</tbody>', '</tr>')
    }
  }

  # add metadata:
  table_str = paste0(ifelse(all(is_graded),
                            '<table>',
                            paste0('<table border="1" width = ', width, '%>',
                                   rep(paste0('<col style="width:', round(100/(cols+ 1)), '%">'), cols+1))),
                     '<caption style="caption-side: top">', caption, '</caption>',
                     table_str, '\n',
                     '</table>', '\n')

  # return table as string:
  return(table_str)
}

#' @title Parse matrix as table with default partial points.
#'
#' @description Parse matrix as table with default partial points.
#'
#' @param m [\code{matrix}]\cr Matrix which should be parsed.
#' @param accepted_error [\code{number}]\cr Accepted error. Default is NULL for
#' no accepted error.
#' @param partial_point [\code{number}]\cr Point for partially correct answer.
#' Default is NULL for no partial point.
#' @param paf [\code{number}]\cr factor multiplied with accepted_error for.
#' @param ... See parameters of parse_table.
#'
#' @return Matrix as character with default partial points.
#'
#' @examples
#' parse_table_NM_partial(m = diag(1), accepted_error = 0.01)
#'
#' @export
parse_table_NM_partial = function(m, accepted_error = 0, partial_point = 75, paf = 10, ...) {
  assert_number(x = paf, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE)
  if(accepted_error > 0) {
    return(parse_table(m = m, accepted_error = accepted_error, partial_answer = m,
                       partial_point = 75, partial_accept = accepted_error * paf, ...))
  } else {
    return(parse_table(m = m, accepted_error = accepted_error, ...))
  }
}
