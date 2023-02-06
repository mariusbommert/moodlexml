# type for question:
question_type = function(type) {
  return(paste0('<question type="', type, '">'))
}

# name for question:
question_name = function(name) {
  return(paste0('<name>
                <text>', name, '</text>
                </name>'))
}

# text for question:
question_text = function(text, file = NULL) {
  qt = paste0('<questiontext format="html">
              <text><![CDATA[',
              p_left(text),
              ']]></text>')
  if(!is.null(file)) {
    qt = paste0(qt, file)
  }
  qt = paste0(qt, '</questiontext>')
  return(qt)
}

# metadata for question:
question_meta = function(grade = 1, penalty = 1, hidden = 0) {
  return(paste0('<generalfeedback format="html">
                <text></text>
                </generalfeedback>
                <defaultgrade>', grade, '</defaultgrade>
                <penalty>', penalty, '</penalty>
                <hidden>', hidden, '</hidden>
                <idnumber></idnumber>'))
}

# feedback text:
question_feedback = function() {
  return(paste0('<correctfeedback format="html">
                <text>Your answer is correct.</text>
                </correctfeedback>
                <partiallycorrectfeedback format="html">
                <text>Your answer is partially correct.</text>
                </partiallycorrectfeedback>
                <incorrectfeedback format="html">
                <text>Your answer is incorrect.</text>
                </incorrectfeedback>
                <shownumcorrect/>'))
}
