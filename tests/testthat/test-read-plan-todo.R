context("PLAN TODO functions")

skip_if_not_installed("yaml")

plan <- read_plan(plan)
todo <- read_todo(todo)

test_that( "The correct number of plan items are captured", {

  expect_length(plan, 2)
  expect_length(plan[[1]]$issue, 3)
  expect_length(plan[[2]]$issue, 2)
  expect_length(plan[[1]]$issue[[1]]$assignees, 1)
  expect_length(plan[[1]]$issue[[1]]$labels, 3)
  expect_length(plan[[1]]$issue[[2]]$assignees, 0)
  expect_length(plan[[1]]$issue[[2]]$labels, 1)
  expect_length(plan[[1]]$issue[[3]]$assignees, 0)
  expect_length(plan[[1]]$issue[[3]]$labels, 0)

})

test_that( "The correct number of todo items are captured", {

  expect_length(todo, 2)
  expect_length(todo[[1]], 3)
  expect_length(todo[[2]], 1)

})

test_that( "Correct field names are preserved", {

  expect_named(plan[[1]], c("title", "description", "due_on", "issue"))
  expect_named(plan[[2]], c("title", "description", "issue"))
  expect_named(todo[[1]], c("title", "body", "labels"))
  expect_named(todo[[2]], c("title"))

})

test_that( "Correct class labels are applied", {

  expect_equal(class(plan)[1], "plan" )
  expect_equal(class(todo)[1], "todo" )

})

test_that( "Correct class labels are applied", {

  expect_s3_class(plan, "plan" )
  expect_s3_class(todo, "todo" )

})

