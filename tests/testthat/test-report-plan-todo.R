context("REPORT PLAN TODO functions")

plan <- read_plan(plan)

test_that( "The correct number of plan items are captured", {

  expect_equal( length(plan), 2)
  expect_equal( length(plan[[1]]$issue), 3)
  expect_equal( length(plan[[2]]$issue), 2)
  expect_equal( length(plan[[1]]$issue[[1]]$assignees), 1)
  expect_equal( length(plan[[1]]$issue[[1]]$labels), 3)
  expect_equal( length(plan[[1]]$issue[[2]]$assignees), 0)
  expect_equal( length(plan[[1]]$issue[[2]]$labels), 1)
  expect_equal( length(plan[[1]]$issue[[3]]$assignees), 0)
  expect_equal( length(plan[[1]]$issue[[3]]$labels), 0)

})
