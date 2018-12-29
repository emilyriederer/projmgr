context("GET functions")

skip_on_cran()
skip_on_travis()
skip_on_appveyor()

experigit <- create_repo_ref('emilyriederer', 'experigit')

test_that( "zero item output is returned as type character (vs list)", {

  expect_type( get_engine("/issues", experigit, since = '4000-12-31'), "character")
  expect_type( get_issues(experigit, since = '4000-12-31'), "character")

})

test_that( "single item output is wrapped into list of length 1", {

  expect_equal( length(get_engine("/issues/1", experigit)), 1)
  expect_equal( length(get_issues(experigit, 1)), 1)
  expect_equal( length(get_milestones(experigit, number = 10)), 1)

})

test_that( "get_issues gives message when hitting page limit", {

  expect_message( get_issues(experigit, limit = 1, state = "all"))

})
