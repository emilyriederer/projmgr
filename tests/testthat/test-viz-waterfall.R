context("VIZ WATERFALL function")

skip_if_not_installed("ggplot2")

all_open <- data.frame(
  state = c("open", "open", "open", "open"),
  created_at = c("2018-01-01", "2018-03-01", "2018-07-01", "2018-12-01"),
  closed_at = c(NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

all_closed <- data.frame(
  state = c("closed", "closed", "closed", "closed"),
  created_at = c("2018-01-01", "2018-03-01", "2018-07-01", "2018-12-01"),
  closed_at = c("2018-06-30", "2018-06-30", "2018-09-30", "2018-12-31"),
  stringsAsFactors = FALSE
)

mixed <- data.frame(
  state = c("open", "closed", "open", "closed"),
  created_at = c("2018-01-01", "2018-03-01", "2018-07-01", "2018-12-01"),
  closed_at = c(NA, "2018-06-30", NA, "2018-12-31"),
  stringsAsFactors = FALSE
)

o1 <- viz_waterfall(all_open, "2017-01-01", "2019-01-01")
o2 <- viz_waterfall(all_open, "2018-02-01", "2019-01-01")
o3 <- viz_waterfall(all_open, "2018-06-01", "2019-01-01")
o4 <- viz_waterfall(all_open, "2018-09-01", "2019-01-01")

c1 <- viz_waterfall(all_closed, "2017-01-01", "2019-01-01")
c2 <- viz_waterfall(all_closed, "2018-02-01", "2019-01-01")
c3 <- viz_waterfall(all_closed, "2018-06-01", "2019-01-01")
c4 <- viz_waterfall(all_closed, "2018-09-01", "2019-01-01")

m1 <- viz_waterfall(mixed, "2017-01-01", "2019-01-01")
m2 <- viz_waterfall(mixed, "2018-02-01", "2019-01-01")
m3 <- viz_waterfall(mixed, "2018-06-01", "2019-01-01")
m4 <- viz_waterfall(mixed, "2018-09-01", "2019-01-01")

test_that("Tally is correct when all items are open",
  {
  expect_equal(ggplot2::ggplot_build(o1)$plot$data$n, c(0,4,0,4))
  expect_equal(ggplot2::ggplot_build(o2)$plot$data$n, c(1,3,0,4))
  expect_equal(ggplot2::ggplot_build(o3)$plot$data$n, c(2,2,0,4))
  expect_equal(ggplot2::ggplot_build(o4)$plot$data$n, c(3,1,0,4))
  })

test_that("Tally is correct when all items are closed",
  {
    expect_equal(ggplot2::ggplot_build(c1)$plot$data$n, c(0,4,4,0))
    expect_equal(ggplot2::ggplot_build(c2)$plot$data$n, c(1,3,4,0))
    expect_equal(ggplot2::ggplot_build(c3)$plot$data$n, c(2,2,4,0))
    expect_equal(ggplot2::ggplot_build(c4)$plot$data$n, c(1,1,2,0))
  })

test_that("Tally is correct when all items contain mixed of open and closed",
  {
    expect_equal(ggplot2::ggplot_build(m1)$plot$data$n, c(0,4,2,2))
    expect_equal(ggplot2::ggplot_build(m2)$plot$data$n, c(1,3,2,2))
    expect_equal(ggplot2::ggplot_build(m3)$plot$data$n, c(2,2,2,2))
    expect_equal(ggplot2::ggplot_build(m4)$plot$data$n, c(2,1,1,2))
  })
