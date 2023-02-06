#' @title Write single Moodle question to XML.
#'
#' @description Write a single Moodle question to an XML-file.
#'
#' @param question [\code{character}]\cr Question as character.
#' @param name [\code{character}]\cr Name of XML-file.
#' @param force [\code{flag}]\cr TRUE to force writing. Default is FALSE.
#'
#' @return Nothing is returned but an XML-file is written.
#'
#' @examples
#' \dontrun{
#' write_question_xml(question = description(name = "Description", text = "text"),
#'                    name = "description.xml")
#' }
#'
#' @export
write_question_xml = function(question, name, force = FALSE) {
  # check input:
  assert_character(x = question, any.missing = FALSE, len = 1)
  assert_character(x = name, any.missing = FALSE, len = 1)
  assert_flag(x = force)

  # write question to xml:
  if(!file.exists(name) || force) {
    filecon = file(name)
    writeLines(text = c('<?xml version="1.0" encoding="UTF-8"?>',
                        '<quiz>', question, '</quiz>'),
               con = filecon, useBytes = TRUE)
    close(con = filecon)
  } else {
    warning(paste0('File ', name, ' already exists. Use force = TRUE to ',
                   'force writing.'))
  }
}

#' @title Write quiz to XML.
#'
#' @description Write a whole quiz to an XML-file.
#'
#' @param name [\code{character}]\cr Name of XML-file.
#' @param category [\code{character}]\cr Name of Moodle category.
#' @param questions [\code{character}]\cr Questions as character.
#' @param force [\code{flag}]\cr TRUE to force writing. Default is FALSE.
#'
#' @return Nothing is returned but an XML-file is written.
#'
#' @examples
#' \dontrun{
#' write_quiz_xml(name = "Quiz.xml", category = "Quiz 1",
#'                questions = description(name = "Description", text = "text"))
#' }
#'
#' @export
write_quiz_xml = function(name, category, questions, force = FALSE) {
  # check input:
  assert_character(x = name, any.missing = FALSE, len = 1)
  assert_character(x = category, any.missing = FALSE, len = 1)
  assert_character(x = questions, any.missing = FALSE, min.len = 1)

  # paste quiz:
  quiz = paste0('<?xml version="1.0" encoding="UTF-8"?>
                <quiz>
                <question type="category">
                <category>
                <text>', category, '</text>
                </category>
                <info format="html">
                <text></text>
                </info>
                <idnumber></idnumber>
                </question>',
                paste0(questions, collapse = ""),
                '</quiz>')

  # write file:
  if(!file.exists(name) || force) {
    filecon = file(name)
    writeLines(text = quiz, con = filecon, useBytes = TRUE)
    close(con = filecon)
  } else {
    warning(paste0('File ', name, ' already exists. Use force = TRUE to ',
                   'force writing.'))
  }
}
