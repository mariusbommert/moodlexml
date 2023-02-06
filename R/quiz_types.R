#' @title Question of type description.
#'
#' @description Generate a question of type description.
#'
#' @param name [\code{character}]\cr Name for description.
#' @param text [\code{character}]\cr Question text for description.
#' @param file [\code{character}]\cr Optional file encoded as base64. Default is
#' NULL for no file.
#' @param hidden [\code{choice}]\cr 0 for show and 1 for hidden.
#'
#' @return Question of type description as character.
#'
#' @examples description(name = "Description", text = "text")
#'
#' @export
description = function(name, text, file = NULL, hidden = 0) {
  # check input:
  assert_character(x = name, any.missing = FALSE, len = 1)
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_character(x = file, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert_choice(x = hidden, choices = c(0, 1))

  # paste question elements for description:
  do = question_type(type = "description")
  n = question_name(name = name)
  q = question_text(text = text, file = file)
  qm = question_meta(grade = 0, penalty = 1, hidden = hidden)
  dc = paste0('</question>')

  # return description:
  return(paste0(do, n, q, qm, dc))
}

################################################################################

# answer for multichoice:
answer_multichoice = function(answer, fraction) {
  paste0('<answer fraction="', fraction, '" format="html">
         <text><![CDATA[', br(p_left(answer)),
         ']]></text>
         <feedback format="html">
         <text></text>
         </feedback>
         </answer>')
}

#' @title Vector with one 100 and 0 otherwise.
#'
#' @description Vector with one 100 at position pos and 0 otherwise.
#'
#' @param n [\code{count}]\cr Total number of values.
#' @param pos [\code{count}]\cr Position of 100.
#'
#' @return Vector with one 100 at position pos and 0 otherwise.
#'
#' @examples zero_100_pos(n = 5, pos = 3)
#'
#' @export
zero_100_pos = function(n, pos) {
  # check input:
  assert_count(x = n, positive = TRUE)
  assert_integerish(x = pos, lower = 1, upper = n, any.missing = FALSE)

  # return vector with one 100 and 0 otherwise:
  return(c(rep(0, pos - 1), 100, rep(0, n - pos)))
}

#' @title Question of type multichoice.
#'
#' @description Generate a question of type multichoice.
#'
#' @param name [\code{character}]\cr Name for multichoice.
#' @param text [\code{character}]\cr Question text for multichoice.
#' @param single [\code{flag}]\cr TRUE for single choice. FALSE for multichoice.
#' @param answers [\code{character}]\cr Vector of answers.
#' @param fractions [\code{character}]\cr Vector of fractions corresponding to
#' answers.
#' @param shuffleanswers [\code{flag}]\cr TRUE to shuffle answers. Default is FALSE.
#' @param answernumbering [\code{choice}]\cr "none", "abc" or "123" for numbering.
#' Default is "none".
#' @param showstandardinstructions [\code{choice}]\cr 0 to hide instructions
#' and 1 to show instructions. Default is 1.
#' @param file [\code{character}]\cr Optional file encoded as base64. Default is
#' NULL for no file.
#' @param grade [\code{number}]\cr Grade for the question. Default is 1.
#' @param penalty [\code{number}]\cr Penalty for the question. Default is 1.
#' @param hidden [\code{choice}]\cr 0 for show and 1 for hidden.
#'
#' @return Question of type multichoice as character.
#'
#' @examples
#' multichoice(name = "Multichoice", text = "Choose a", single = TRUE,
#'             answers = c("a", "b"), fractions = zero_100_pos(n = 2, pos = 1))
#'
#' @export
multichoice = function(name, text, single, answers, fractions,
                       shuffleanswers = FALSE, answernumbering = "none",
                       showstandardinstructions = 1, file = NULL,
                       grade = 1, penalty = 1, hidden = 0) {
  # check input:
  assert_character(x = name, any.missing = FALSE, len = 1)
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_flag(x = single)
  if(!test_character(x = answers)) {
    answers = tryCatch(as.character(answers), error = function(e) stop("invalid answers"))
  }
  assert_character(x = answers, any.missing = FALSE, min.len = 1)
  assert_numeric(x = fractions, lower = -100, upper = 100, len = length(answers),
                 any.missing = FALSE)
  assert_flag(x = shuffleanswers)
  assert_choice(x = answernumbering, choices = c("none", "123", "abc")) # correct, add more?
  assert_choice(x = showstandardinstructions, choices = c(0, 1))
  assert_character(x = file, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert_number(x = grade, lower = 0, finite = TRUE)
  assert_number(x = penalty, lower = 0, upper = 1)
  assert_choice(x = hidden, choices = c(0, 1))

  # paste question elements for multichoice:
  qo = question_type(type = "multichoice")
  n = question_name(name = name)
  q = question_text(text = text, file = file)
  qm = question_meta(grade = grade, penalty = penalty, hidden = hidden)
  m = paste0('<single>',
             ifelse(single, 'true', 'false'),
             '</single>
    <shuffleanswers>', ifelse(shuffleanswers, 'true', 'false'), '</shuffleanswers>
    <answernumbering>', answernumbering, '</answernumbering>
    <showstandardinstruction>', showstandardinstructions, '</showstandardinstruction>')
  qf = question_feedback()
  a = lapply(X = seq_along(answers), FUN = function(x)
    answer_multichoice(answer = answers[[x]], fraction = fractions[x]))
  qc = paste0('</question>')

  # return multichoice:
  return(paste0(qo, n, q, qm, m, qf, paste0(a, collapse = ""), qc))
}

################################################################################

# add partially correct answer:
partial = function(partial_answer, partial_point, partial_accept) {
  if(!is.null(partial_answer) && !is.null(partial_point)) {
    if(is.null(partial_accept) || partial_accept == 0) {
      return(paste0('~%', partial_point, '%', partial_answer))
    } else {
      return(paste0('~%', partial_point, '%', partial_answer, ':', partial_accept))
    }
  }
}

#' @title Cloze answer of type NM, SA or SAC.
#'
#' @description Cloze answer of type NM, SA or SAC.
#'
#' @param type [\code{choice}]\cr Types NM, SA or SAC.
#' @param answer [\code{character}]\cr Correct answer.
#' @param point [\code{number}]\cr Point for correct answer. Default is 1.
#' @param wrong_default [\code{count}]\cr Length of wrong default answer. Default
#' is 7.
#' @param accepted_error [\code{number}]\cr Accepted error. Default is NULL for
#' no accepted error.
#' @param partial_answer [\code{character}]\cr Partially correct answer.
#' @param partial_point [\code{number}]\cr Point for partially correct answer.
#' Default is NULL for no partial point.
#' @param partial_accept [\code{number}]\cr Accepted error for partially correct
#' answer. Default is NULL for no accepted error for partial answer.
#'
#' @return Cloze answer as character.
#'
#' @examples
#' cloze_type(type = "NM", answer = 123)
#'
#' @export
cloze_type = function(type, answer, point = 1, wrong_default = 7, accepted_error = NULL,
                      partial_answer = NULL, partial_point = NULL,
                      partial_accept = NULL) {
  # check input:
  assert_choice(x = type, choices = c("NM", "SA", "SAC"))
  if(!test_character(x = answer)) {
    answer = tryCatch(as.character(answer), error = function(e)
      stop("invalid answer"))
  }
  assert_character(x = answer, any.missing = FALSE, len = 1)
  assert_number(x = point, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE)
  assert_count(x = wrong_default, null.ok = FALSE)
  assert_number(x = accepted_error, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = TRUE)
  if(!test_character(x = partial_answer, any.missing = FALSE, null.ok = TRUE)) {
    partial_answer = tryCatch(as.character(partial_answer), error = function(e)
      stop("invalid partial_answer"))
  }
  assert_character(x = partial_answer, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert_numeric(x = partial_point, lower = 0, len = length(partial_answer),
                 any.missing = FALSE, null.ok = TRUE)
  assert_numeric(x = partial_accept, lower = 0, len = length(partial_answer),
                 any.missing = FALSE, null.ok = TRUE)

  # generate wrong default:
  wrong_default = as.numeric(paste0(1:wrong_default, collapse = ""))

  # paste point, type, answer (and accepted_error):
  if(is.null(accepted_error) || accepted_error == 0) {
    string = paste0('{', point, ':', type, ':=', answer)
  } else {
    string = paste0('{', point, ':', type, ':=', answer, ':', accepted_error)
  }

  # paste partial answers:
  if(!is.null(partial_answer) && !is.null(partial_point)) {
    for(i in seq_along(partial_answer)) {
      string = paste0(string, partial(partial_answer[i], partial_point[i], partial_accept[i]))
    }
  }

  # paste wrong default:
  string = paste0(string, '~%0%', wrong_default, '}')

  # return cloze answer:
  return(string)
}

#' @title Cloze answer of type NM with default partial points.
#'
#' @description Cloze answer of type NM with default partial points.
#'
#' @param answer [\code{character}]\cr Correct answer.
#' @param point [\code{number}]\cr Point for correct answer. Default is 1.
#' @param wrong_default [\code{count}]\cr Length of wrong default answer. Default
#' is 7.
#' @param accepted_error [\code{number}]\cr Accepted error. Default is NULL for
#' no accepted error.
#' @param partial_point [\code{number}]\cr Point for partially correct answer.
#' Default is 75.
#' @param paf [\code{number}]\cr factor multiplied with accepted_error for.
#' @param ... See parameters of cloze_type.
#'
#' @return Cloze answer as character.
#'
#' @examples
#' cloze_NM_partial(answer = 123)
#'
#' @export
cloze_NM_partial = function(answer, point = 1, wrong_default = 7,
                            accepted_error = NULL, partial_point = 75, paf = 10, ...) {
  assert_number(x = paf, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE)
  if(!is.null(accepted_error) && accepted_error > 0) {
    return(cloze_type(type = "NM", answer = answer, point = point,
                      wrong_default = wrong_default, accepted_error = accepted_error,
                      partial_answer = answer, partial_point = partial_point,
                      partial_accept = accepted_error * paf, ...))
  } else {
    return(cloze_type(type = "NM", answer = answer, point = point,
                      wrong_default = wrong_default,
                      accepted_error = accepted_error, partial_answer = NULL, ...))
  }
}

#' @title Single cloze multichoice question with 2 possible answers.
#'
#' @description Single cloze multichoice question with 2 possible answers.
#'
#' @param a [\code{character}]\cr First answer.
#' @param b [\code{character}]\cr Second answer.
#' @param true [\code{choice}]\cr "a" for a TRUE and "b" for b TRUE.
#' @param point [\code{number}]\cr Point for correct answer. Default is 1.
#'
#' @return Cloze multichoice question as character.
#'
#' @examples cloze_mc_ab(a = "Yes", b = "No", true = "a")
#'
#' @export
cloze_mc_ab = function(a, b, true, point = 1) {
  # check input:
  if(!test_character(x = a)) {
    a = tryCatch(as.character(a), error = function(e) stop("invalid a"))
  }
  assert_character(x = a, any.missing = FALSE, len = 1)
  if(!test_character(x = b)) {
    b = tryCatch(as.character(b), error = function(e) stop("invalid a"))
  }
  assert_character(x = b, any.missing = FALSE, len = 1)
  assert_choice(x = true, choices = c("a", "b"))
  assert_number(x = point, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE)

  # return multichoice question:
  return(paste0('{', point, ':MC:', ifelse(true == "a", '=', ''), a, '~',
                ifelse(true == "b", '=', ''), b, '}'))
}

#' @title Single cloze multichoice question with arbitrary number of possible answers.
#'
#' @description Single cloze multichoice question with arbitrary number of possible answers.
#'
#' @param choices [\code{character}]\cr Possible choices.
#' @param pos_true [\code{number}]\cr Position of true answer(s).
#' @param point [\code{number}]\cr Point for correct answer. Default is 1.
#'
#' @return Cloze multichoice question as character.
#'
#' @examples cloze_mc_multi(choices = 1:5, pos_true = 3)
#'
#' @export
cloze_mc_multi = function(choices, pos_true, point = 1) {
  # check input:
  if(!test_character(x = choices)) {
    choices = tryCatch(as.character(choices), error = function(e) stop("invalid choices"))
  }
  assert_character(x = choices, any.missing = FALSE, min.len = 1)
  assert_numeric(x = pos_true, lower = 1, upper = length(choices), max.len = length(choices))
  assert_number(x = point, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE)

  # logical with true answer:
  trues = logical(length(choices))
  trues[pos_true] = TRUE

  # paste choices:
  choices = paste0(ifelse(trues, '=', ''), choices)
  choices[2:length(choices)] = paste0('~', choices[2:length(choices)])

  # return multichoice question:
  return(paste0('{', point, ':MC:', paste0(choices, collapse = ''), '}'))
}

#' @title Multiple cloze multichoice questions with arbitrary number of same
#' possible answers.
#'
#' @description Multiple cloze multichoice questions with arbitrary number of
#' same parameters.
#'
#' @param conditions [\code{conditions}]\cr Conditions.
#' @param choices [\code{character}]\cr Possible choices.
#' @param pos_trues [\code{integerish}]\cr Positions of true answers.
#' @param point [\code{number}]\cr Point for correct answer. Default is 1.
#'
#' @return Cloze multichoice questions.
#'
#' @examples
#' cloze_mc_multi_conditions(conditions = c("a", "b", "c"), choices = 1:5,
#'                           pos_trues = c(1, 3, 5))
#'
#' @export
cloze_mc_multi_conditions = function(conditions, choices, pos_trues, point = 1) {
  # check input:
  if(!test_character(x = conditions)) {
    conditions = tryCatch(as.character(conditions), error = function(e) stop("invalid conditions"))
  }
  assert_character(x = conditions, any.missing = FALSE, min.len = 1)
  if(!test_character(x = choices)) {
    choices = tryCatch(as.character(choices), error = function(e) stop("invalid choices"))
  }
  assert_character(x = choices, any.missing = FALSE, min.len = 1)
  assert_integerish(x = pos_trues, lower = 1, upper = length(choices),
                    any.missing = FALSE, len = length(conditions))

  # generate multichoice questions:
  tmp = purrr::map2(conditions, pos_trues, function(x, y)
    p(paste0(x, ' ', cloze_mc_multi(choices = choices, pos_true = y, point = point))))

  # return multichoice questions:
  return(paste0(tmp, collapse = ""))
}

#' @title Multiple cloze multichoice questions with arbitrary number of same
#' possible answers as list.
#'
#' @description Multiple cloze multichoice questions with arbitrary number of same
#' possible answers as list.
#'
#' @param conditions [\code{conditions}]\cr Conditions.
#' @param choices [\code{character}]\cr Possible choices.
#' @param pos_trues [\code{integerish}]\cr Positions of true answers.
#' @param ordered [\code{flag}]\cr TRUE for ordered list, FALSE for unordered list,
#' default is FALSE.
#'
#' @examples
#' cloze_mc_list(conditions = c("a", "b", "c"), choices = 1:5,
#'               pos_trues = c(1, 3, 5))
#'
#' @export
cloze_mc_list = function(conditions, choices, pos_trues, ordered = FALSE) {
  # check input:
  if(!test_character(x = conditions)) {
    conditions = tryCatch(as.character(conditions), error = function(e) stop("invalid conditions"))
  }
  assert_character(x = conditions, any.missing = FALSE, min.len = 1)
  if(!test_character(x = choices)) {
    choices = tryCatch(as.character(choices), error = function(e) stop("invalid choices"))
  }
  assert_character(x = choices, any.missing = FALSE, min.len = 1)
  assert_integerish(x = pos_trues, lower = 1, upper = length(choices),
                    any.missing = FALSE)
  assert_flag(ordered)

  # paste parts to get multichoices as list:
  tmp = purrr::map(pos_trues, function(x) cloze_mc_multi(choices = choices, pos_true = x))
  tmp = purrr::map2(conditions, tmp, function(x, y) li(paste0(x, '&nbsp;', y)))
  tmp = paste0('<',ifelse(ordered, 'o', 'u'), 'l>', paste0(tmp, collapse = ""),
               '</', ifelse(ordered, 'o', 'u'), 'l>')

  # return multichoice questions:
  return(tmp)
}

#' @title Question of type cloze.
#'
#' @description Generate a question of type cloze.
#'
#' @param name [\code{character}]\cr Name for cloze.
#' @param text [\code{character}]\cr Question text for cloze.
#' @param file [\code{character}]\cr Optional file encoded as base64. Default is
#' NULL for no file.
#' @param grade [\code{number}]\cr Grade for the question. Default is 1.
#' @param penalty [\code{number}]\cr Penalty for the question. Default is 1.
#' @param hidden [\code{choice}]\cr 0 for show and 1 for hidden.
#'
#' @return Question of type cloze as character.
#'
#' @examples
#' cloze(name = "Cloze", text = cloze_mc_multi(choices = 1:5, pos_true = 3))
#'
#' @export
cloze = function(name, text, file = NULL, grade = 1, penalty = 1, hidden = 0) {
  # check input:
  assert_character(x = name, any.missing = FALSE, len = 1)
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_character(x = file, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert_number(x = grade, lower = 0, finite = TRUE)
  assert_number(x = penalty, lower = 0, upper = 1)
  assert_choice(x = hidden, choices = c(0, 1))

  # paste question elements for type cloze:
  qo = question_type(type = "cloze")
  n = question_name(name = name)
  q = question_text(text = text, file = file)
  qm = question_meta(grade = grade, penalty = penalty, hidden = hidden)
  qf = question_feedback()
  qc = paste0('</question>')

  # return cloze:
  return(paste0(qo, n, q, qm, qf, qc))
}

################################################################################

#' @title Question of type essay.
#'
#' @description Generate a question of type essay.
#'
#' @param name [\code{character}]\cr Name for essay.
#' @param text [\code{character}]\cr Question text for essay.
#' @param file [\code{character}]\cr Optional file encoded as base64. Default is
#' NULL for no file.
#' @param grade [\code{number}]\cr Grade for the question. Default is 1.
#' @param penalty [\code{number}]\cr Penalty for the question. Default is 1.
#' @param hidden [\code{choice}]\cr 0 for show and 1 for hidden.
#' @param responseformat [\code{choice}]\cr "plain" or "editor". Default is
#' editor.
#' @param responserequired [\code{choice}]\cr 0 for no response required and 1
#' for response required. Default is 0.
#' @param responsefieldlines [\code{count}]\cr Number of lines for responsefield.
#' @param minwordlimit [\code{character}]\cr Minimum word limit for response. Default
#' is "" for no limit.
#' @param maxwordlimit [\code{character}]\cr Maximum word limit for response. Default
#' is "" for no limit.
#' @param attachments [\code{count}]\cr Number of allowed attachments. Default is 1.
#' @param attachmentsrequired [\code{count}]\cr Number of required attachments.
#' Default is 1.
#' @param maxbytes [\code{count}]\cr Maximum file size in bytes. Default is 0 for
#' no limit.
#' @param filetypeslist [\code{character}]\cr File types seperated by ", ". Default
#' is ".R".
#'
#' @return Question of type essay as character.
#'
#' @examples essay(name = "Essay", text = "Upload your file!")
#'
#' @export
essay = function(name, text, file = NULL, grade = 0, penalty = 1, hidden = 0,
                 responseformat = "editor", responserequired = 0,
                 responsefieldlines = 5, minwordlimit = "", maxwordlimit = "",
                 attachments = 1, attachmentsrequired = 1, maxbytes = 0,
                 filetypeslist = ".R") { # ".R, .RData, .Rmd"
  # check input:
  assert_character(x = name, any.missing = FALSE, len = 1)
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_character(x = file, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert_number(x = grade, lower = 0, finite = TRUE)
  assert_number(x = penalty, lower = 0, upper = 1)
  assert_choice(x = hidden, choices = c(0, 1))
  assert_choice(x = responseformat, choices = c("plain", "editor")) # add more choices
  assert_choice(x = responserequired, choices = c(0, 1)) # correct?
  assert_count(x = responsefieldlines, positive = TRUE)
  assert_character(x = minwordlimit, any.missing = FALSE, len = 1) # change assertion?
  assert_character(x = maxwordlimit, any.missing = FALSE, len = 1) # change assertion?
  assert_count(x = attachments)
  assert_count(x = attachmentsrequired)
  assert_count(x = maxbytes)
  assert_character(x = filetypeslist, any.missing = FALSE, len = 1) # change assertion?

  # paste question elements for type essay:
  qo = question_type(type = "essay")
  n = question_name(name = name)
  q = question_text(text = text, file = file)
  qm = question_meta(grade = grade, penalty = penalty, hidden = hidden)
  m = paste0('<responseformat>', responseformat, '</responseformat>',
             '<responserequired>', responserequired, '</responserequired>',
             '<responsefieldlines>', responsefieldlines, '</responsefieldlines>',
             '<minwordlimit>', minwordlimit, '</minwordlimit>',
             '<maxwordlimit>', maxwordlimit, '</maxwordlimit>',
             '<attachments>', attachments, '</attachments>',
             '<attachmentsrequired>', attachmentsrequired, '</attachmentsrequired>',
             '<maxbytes>', maxbytes, '</maxbytes>',
             '<filetypeslist>', filetypeslist, '</filetypeslist>',
             '<graderinfo format="html">',
             '<text></text>',
             '</graderinfo>',
             '<responsetemplate format="html">',
             '<text></text>',
             '</responsetemplate>')
  qc = paste0('</question>')

  # return essay:
  return(paste0(paste0(qo, n, q, qm, m, qc)))
}

################################################################################

#' @title Drag item.
#'
#' @description Drag item.
#'
#' @param number [\code{count}]\cr Number of drag item.
#' @param text [\code{character}]\cr Text of drag item.
#' @param draggroup [\code{count}]\cr Group of drag item.
#' @param infinite [\code{flag}]\cr TRUE for infinite replicates of drag item.
#' FALSE for only one drag item. Default is TRUE.
#'
#' @return Drag item as character.
#'
#' @examples drag(number = 1, text = 1, draggroup = 1)
#'
#' @export
drag = function(number, text, draggroup, infinite = TRUE) {
  # check input:
  assert_count(x = number, positive = TRUE)
  if(!test_character(x = text)) {
    text = tryCatch(as.character(text), error = function(e) stop("invalid text"))
  }
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_count(x = draggroup, positive = TRUE)
  assert_flag(x = infinite)

  # paste drag item:
  dr = paste0('<drag>',
              '<no>', number, '</no>',
              '<text>', text, '</text>',
              '<draggroup>', draggroup, '</draggroup>',
              ifelse(infinite, '<infinite/>', ''),
              '</drag>')

  # return drag item:
  return(dr)
}

#' @title Drop zone.
#'
#' @description Drop zone.
#'
#' @param number [\code{count}]\cr Number of drop zone.
#' @param choice [\code{count}]\cr Number of correct drag item.
#' @param xleft [\code{number}]\cr Position of drop zone on x-axis.
#' @param ytop [\code{number}]\cr Position of drop zone on y-axis.
#'
#' @return Drop zone as character.
#'
#' @examples drop(number = 1, choice = 1, xleft = 1, ytop = 1)
#'
#' @export
drop = function(number, choice, xleft, ytop) {
  # check input:
  assert_count(x = number, positive = TRUE)
  assert_count(x = choice, positive = TRUE)
  assert_number(x = xleft, finite = TRUE)
  assert_number(x = ytop, finite = TRUE)

  # paste drop item:
  dr = paste0('<drop>',
              '<text></text>',
              '<no>', number, '</no>',
              '<choice>', choice, '</choice>',
              '<xleft>', xleft, '</xleft>',
              '<ytop>', ytop, '</ytop>',
              '</drop>')

  # return drop item:
  return(dr)
}

#' @title Question of type ddimageortext.
#'
#' @description Generate a question of type ddimageortext.
#'
#' @param name [\code{character}]\cr Name for ddimageortext.
#' @param text [\code{character}]\cr Question text for ddimageortext.
#' @param image_name [\code{character}]\cr Name for background image.
#' @param drags [\code{character}]\cr Drag items.
#' @param drops [\code{character}]\cr Drop zones.
#' @param file [\code{character}]\cr Optional file encoded as base64. Default is
#' NULL for no file.
#' @param grade [\code{number}]\cr Grade for the question. Default is 1.
#' @param penalty [\code{number}]\cr Penalty for the question. Default is 1.
#' @param hidden [\code{choice}]\cr 0 for show and 1 for hidden.
#'
#' Question of type ddimageortext as character.
#'
#' @examples
#' ddimageortext(name = "ddimageortext", text = "Drag and drop",
#'               image_name = read_example("text.png"),
#'               drags = drag(number = 1, text = 1, draggroup = 1),
#'               drops = drop(number = 1, choice = 1, xleft = 1, ytop = 1))
#'
#' @export
ddimageortext = function(name, text, image_name, drags, drops, file = NULL,
                         grade = 1, penalty = 1, hidden = 0) {
  # check input:
  assert_character(x = name, any.missing = FALSE, len = 1)
  assert_character(x = text, any.missing = FALSE, len = 1)
  assert_character(x = image_name, any.missing = FALSE, len = 1)
  assert_character(x = drags, any.missing = FALSE, len = 1)
  assert_character(x = drops, any.missing = FALSE, len = 1)
  assert_character(x = file, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert_number(x = grade, lower = 0, finite = TRUE)
  assert_number(x = penalty, lower = 0, upper = 1)
  assert_choice(x = hidden, choices = c(0, 1))

  # paste question elements for type ddimageortext:
  qo = question_type(type = "ddimageortext")
  n = question_name(name = name)
  q = question_text(text = text, file = file)
  qm = question_meta(grade = grade, penalty = penalty, hidden = hidden)
  qf = question_feedback()
  b = insert_base64(name = image_name)
  qc = paste0('</question>')

  # return ddimageortext:
  return(paste0(qo, n, q, qm, qf, b, drags, drops, qc))
}
