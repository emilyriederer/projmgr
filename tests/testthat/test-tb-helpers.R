context("TASKBOARD functions")

test_that( "Issue-specific taskboard helpers evaluate correctly with expected input", {

  expect_equal( is_labeled()(df_issue), c(FALSE, TRUE, TRUE))
  expect_equal( is_labeled_with("a")(df_issue), c(FALSE, TRUE, TRUE))
  expect_equal( is_labeled_with("b")(df_issue), c(FALSE, FALSE, TRUE))
  expect_equal( is_labeled_with(c("b", "c"))(df_issue), c(FALSE, FALSE, TRUE))
  expect_equal( is_labeled_with(c("b", "c"), any = FALSE)(df_issue), c(FALSE, FALSE, FALSE))
  expect_equal( is_labeled_with("c")(df_issue), c(FALSE, FALSE, FALSE))

  expect_equal( is_assigned()(df_issue), c(FALSE, TRUE, TRUE))
  expect_equal( is_assigned_to("a")(df_issue), c(FALSE, TRUE, TRUE))
  expect_equal( is_assigned_to("b")(df_issue), c(FALSE, FALSE, TRUE))
  expect_equal( is_assigned_to(c("b", "c"))(df_issue), c(FALSE, FALSE, TRUE))
  expect_equal( is_assigned_to(c("b", "c"), any = FALSE)(df_issue), c(FALSE, FALSE, FALSE))
  expect_equal( is_assigned_to("c")(df_issue), c(FALSE, FALSE, FALSE))

  expect_equal( is_in_a_milestone()(df_issue), c(TRUE, FALSE, TRUE))
  expect_equal( is_in_milestone(2)(df_issue), c(FALSE, FALSE, TRUE))
  expect_equal( is_in_milestone(5)(df_issue), c(FALSE, FALSE, FALSE))

  expect_equal( is_created_before("2018-12-01")(df_issue), c(FALSE, FALSE, FALSE))
  expect_equal( is_created_before("2019-03-01")(df_issue), c(TRUE, FALSE, FALSE))
  expect_equal( is_created_before("2019-09-01")(df_issue), c(TRUE, TRUE, FALSE))
  expect_equal( is_created_before("2020-03-01")(df_issue), c(TRUE, TRUE, TRUE))

  expect_equal( has_n_commits(df_events)(df_issue), c(TRUE, TRUE, FALSE))
  expect_equal( has_n_commits(df_events, n = 2)(df_issue), c(TRUE, FALSE, FALSE))
  expect_equal( has_n_commits(df_events, n = 3)(df_issue), c(FALSE, FALSE, FALSE))

})

test_that( "Milestone-specific taskboard helpers evaluate correctly with expected input", {

  expect_equal( is_due()(df_milestone), c(FALSE, FALSE, TRUE))
  expect_equal( is_due_before("2020-01-01")(df_milestone), c(FALSE, FALSE, TRUE))
  expect_equal( is_due_before("2019-06-01")(df_milestone), c(FALSE, FALSE, FALSE))
  expect_equal( is_part_closed()(df_milestone), c(TRUE, TRUE, FALSE))

})
