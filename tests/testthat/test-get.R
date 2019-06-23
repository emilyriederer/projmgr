context("GET functions")

skip_if_not(interactive(), "This set of tests should only be run manually")

experigit <- create_repo_ref('emilyriederer', 'experigit')
projmgr <- create_repo_ref('emilyriederer', 'projmgr')

test_that( "zero item output is returned as type character (vs list)", {

  expect_type( projmgr:::get_engine("/issues", experigit, since = '4000-12-31'), "character")
  expect_type( get_issues(experigit, since = '4000-12-31'), "character")

})

test_that( "single item output is wrapped into list of length 1", {

  expect_equal( length(projmgr:::get_engine("/issues/1", experigit)), 1)
  expect_equal( length(get_issues(experigit, number = 1)), 1)

})

test_that( "get_issues gives message when hitting page limit", {

  expect_message( get_issues(experigit, limit = 1, state = "all"))

})

test_that( "Issue number appended to events / comments only when positive length", {

  expect_true( all(vapply( get_issue_comments(projmgr, 9) , function(x) "number" %in% names(x), logical(1))) )
  expect_null( names( get_issue_comments(projmgr, 1) ))

})



