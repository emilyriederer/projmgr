context("REPO REF function")

# preserve orig environ vars to reset at end
GITHUB_PAT <- Sys.getenv("GITHUB_PAT")
GITHUB_ENT_PAT <- Sys.getenv("GITHUB_ENT_PAT")

# wipe out enviorn vars
Sys.setenv("GITHUB_PAT" = "")
Sys.setenv("GITHUB_ENT_PAT" = "")

test_that(
  "Error thrown if insufficient credentials supplied", {
  expect_error(create_repo_ref("a", "b"))
  expect_error(create_repo_ref("a", "b", is_ent = TRUE))
  expect_error(create_repo_ref("a", "b", is_ent = TRUE, hostname = "z.com"))
})

# add fake enviorn vars for testing
Sys.setenv("GITHUB_PAT" = "a")
Sys.setenv("GITHUB_ENT_PAT" = "b")

test_that(
  "Correct credentials are inferred from user inputs", {
  expect_equal(create_repo_ref("a", "b")$id, "GITHUB_PAT")
  expect_equal(create_repo_ref("a", "b", is_ent = TRUE, hostname = "z.com")$id, "GITHUB_ENT_PAT")
  expect_equal(create_repo_ref("a", "b", id = "ZZZ")$id, "ZZZ")
  })

# reset environ vars
Sys.setenv("GITHUB_PAT" = GITHUB_PAT)
Sys.setenv("GITHUB_ENT_PAT" = GITHUB_ENT_PAT)
