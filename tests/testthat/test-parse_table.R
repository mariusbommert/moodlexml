test_that("parse_table", {
  expect_equal(parse_table(m = diag(1)), "<table><caption style=\"caption-side: top\"></caption><tr><tbody><td>{1:NM:=1~%0%1234567}</td></tbody></tr>\n</table>\n")

  expect_equal(parse_table_NM_partial(m = diag(1), accepted_error = 0.01),
               "<table><caption style=\"caption-side: top\"></caption><tr><tbody><td>{1:NM:=1:0.01~%75%1:0.1~%0%1234567}</td></tbody></tr>\n</table>\n")
})
