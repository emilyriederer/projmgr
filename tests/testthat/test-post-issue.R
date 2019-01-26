context("POST ISSUES functions")

skip_if_not(interactive(), "This set of tests should only be run manually")
skip_on_os("windows")
skip_on_os("mac")
skip_on_os("linux")
skip_on_cran()
skip_on_travis()
skip_on_appveyor()

experigit <- create_repo_ref('emilyriederer', 'experigit')

# post issues
issue1 <- post_issue(experigit, title = "0 labels, 1 assignee", assignees = 'emilyriederer')
issue2 <- post_issue(experigit, title = "1 label, 1 assignee", labels = 'bug', assignees = 'emilyriederer')
issue3 <- post_issue(experigit, title = "2 labels, 0 assignees", labels = c('bug', 'enhancement'))
issue4 <- post_issue(experigit, title = "2 labels, 0 assignees", distinct = FALSE )

# get issues
issue1_res <- get_issues(experigit, number = issue1)
issue2_res <- get_issues(experigit, number = issue2)
issue3_res <- get_issues(experigit, number = issue3)

test_that( "Multifield values (labels / assignees) are handles correctly for any number of inputs", {

  expect_length( issue1_res[[1]]$labels , 0)
  expect_length( issue2_res[[1]]$labels , 1)
  expect_length( issue3_res[[1]]$labels , 2)
  expect_length( issue2_res[[1]]$assignees , 1)
  expect_length( issue3_res[[1]]$assignees , 0)

})

test_that( "Posting fails appropriately with invalid inputs" , {

  expect_error( post_issue(experigit, body = "I have no title") )
  expect_error( post_issue(experigit, abc = "This is not a valid field name"))

})

test_that( "distinct parameter behavior controls duplication correctly", {

  expect_error( post_issue(experigit, title = "2 labels, 0 assignees") )
  expect_error( post_issue(experigit, title = "2 labels, 0 assignees", distinct = TRUE ) )
  expect_type( issue4, "integer" )

})

# clean-up environment
projmgr:::patch_engine(paste0("/issues/", issue1), ref = experigit, state = 'closed')
projmgr:::patch_engine(paste0("/issues/", issue2), ref = experigit, state = 'closed')
projmgr:::patch_engine(paste0("/issues/", issue3), ref = experigit, state = 'closed')
projmgr:::patch_engine(paste0("/issues/", issue4), ref = experigit, state = 'closed')

