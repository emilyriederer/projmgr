context("PARSE functions")

skip_on_cran()
skip_on_travis()
skip_on_appveyor()

experigit <- create_repo_ref("emilyriederer", "experigit")

test_that( "All parse functions throw error for character (zero-length) output", {

  expect_error( parse_issues(""), "Results object contains no elements to parse." )
  expect_error( parse_issue_comments(""), "Results object contains no elements to parse." )
  expect_error( parse_issue_events(""), "Results object contains no elements to parse." )
  expect_error( parse_milestones(""), "Results object contains no elements to parse." )
  expect_error( parse_repo_labels(""), "Results object contains no elements to parse." )

})

