context("PARSE functions")

issues <- parse_issues(res)

test_that( "All parse functions throw error for character (zero-length) output", {

  expect_error( parse_issues(""), "Results object contains no elements to parse." )
  expect_error( parse_issue_comments(""), "Results object contains no elements to parse." )
  expect_error( parse_issue_events(""), "Results object contains no elements to parse." )
  expect_error( parse_milestones(""), "Results object contains no elements to parse." )
  expect_error( parse_repo_labels(""), "Results object contains no elements to parse." )

})

test_that( "Date fields are correctly parsed as YYYY-MM-DD", {

  expect_true( all(issues$created_at[issues$state == "open"] == "2019-01-26")  )
  expect_true( all(issues$milestone_created_at == "2019-01-26")  )
  expect_true( all(issues$closed_at[issues$state == "closed"] == "2019-01-26" ) )
  expect_true( all(is.na(issues$closed_at[issues$state == "open"])))

})

test_that( "Length 0, 1, or 2 labels and assignees are converted to appropriately sized list", {

  expect_length( issues$labels_name[[1]], 2)
  expect_length( issues$labels_name[[2]], 1)
  expect_length( issues$labels_name[[3]], 0)
  expect_length( issues$labels_name[[4]], 2)
  expect_length( issues$labels_name[[5]], 1)
  expect_length( issues$labels_name[[6]], 0)
  expect_length( issues$assignees_login[[1]], 0)
  expect_length( issues$assignees_login[[2]], 1)
  expect_length( issues$assignees_login[[3]], 1)
  expect_length( issues$assignees_login[[4]], 0)
  expect_length( issues$assignees_login[[5]], 1)
  expect_length( issues$assignees_login[[6]], 1)

})

