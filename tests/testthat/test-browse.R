context("BROWSE functions")

skip_if_offline()
skip_if_not_installed("httr")

url1 <- "https://developer.github.com/v3/issues/milestones/#list-milestones-for-a-repository/"
url2 <- "https://developer.github.com/v3/issues/#list-issues-for-a-repository/"
url3 <- "https://developer.github.com/v3/issues/events/#list-events-for-an-issue/"
url4 <- "https://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository/"
url5 <- "https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue/"
url6 <- "https://developer.github.com/v3/issues/milestones/#create-a-milestone/"
url7 <- "https://developer.github.com/v3/issues/#create-an-issue/"
url8 <- "https://developer.github.com/v3/issues/labels/#create-a-label/"
url9 <- "https://developer.github.com/v3/issues/comments/#create-a-comment/"


test_that("Links to documentation are valid", {
  expect_equal(httr::GET(url1)$status, 200)
  expect_equal(httr::GET(url2)$status, 200)
  expect_equal(httr::GET(url3)$status, 200)
  expect_equal(httr::GET(url4)$status, 200)
  expect_equal(httr::GET(url5)$status, 200)
  expect_equal(httr::GET(url6)$status, 200)
  expect_equal(httr::GET(url7)$status, 200)
  expect_equal(httr::GET(url8)$status, 200)
  expect_equal(httr::GET(url9)$status, 200)
}
)
