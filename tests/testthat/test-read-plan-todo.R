context("PLAN TODO functions")

plan <- read_plan(plan)
todo <- read_todo(todo)

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

test_that( "The correct number of todo items are captured", {

  expect_equal( length(todo), 2)
  expect_equal( length(todo[[1]]), 3)
  expect_equal( length(todo[[2]]), 1)

})

test_that( "Correct field names are preserved", {

  expect_equal( names(plan[[1]]), c("title", "description", "due_on", "issue"))
  expect_equal( names(plan[[2]]), c("title", "description", "issue"))
  expect_equal( names(todo[[1]]), c("title", "body", "labels"))
  expect_equal( names(todo[[2]]), c("title"))

})

test_that( "Correct class labels are applied", {

  expect_equal( class(plan)[1], "plan" )
  expect_equal( class(todo)[1], "todo" )

})

