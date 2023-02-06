test_that("html_tags", {
  expect_equal(tag(text = "text", tag = "p"), "<p>text</p>")

  expect_equal(p_left(text = "text"), "<p dir=\"ltr\" style=\"text-align: left;\">text</p>")

  expect_equal(p(text = "text"), "<p>text</p>")

  expect_equal(b(text = "text"), "<b>text</b>")

  expect_equal(i(text = "text"), "<i>text</i>")

  expect_equal(br(text = "text"), "text <br>")

  expect_equal(li(text = "text"), "<li>text</li>")

  expect_equal(col_rgb(text = "text"), "<span class=\"\" style=\"color: rgb(255, 0, 0);\">text</span>")

  expect_equal(oul(items = 1:2), "<ul><li>1</li><li>2</li></ul>")

  expect_equal(link_file(name = read_example("text.txt"), text = "text.txt"),
               "<a href=\"@@PLUGINFILE@@/text.txt\">text.txt</a>")
})
