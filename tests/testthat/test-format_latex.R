test_that("format_latex", {
  expect_equal(pmatrix(values = c(1, 2)), "\\begin{pmatrix}1 \\\\ 2 \\end{pmatrix}")
  expect_equal(pmatrix(values = diag(2)), "\\begin{pmatrix} 1 & 0\\\\ 0 & 1 \\end{pmatrix}")

  expect_equal(pd(text = 1:2), c("$$1$$", "$$2$$"))

  expect_equal(read_latex(file = read_example("text.txt"), add_p = FALSE),
               c("a = 1", "b = 2", "c = 3"))
})


