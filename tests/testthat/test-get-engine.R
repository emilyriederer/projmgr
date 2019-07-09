context("GET ENGINE functions")

repo_ref <- list(repo_owner = "abc",
                 repo_name = "xyz",
                 id = "GITHUB_PAT",
                 pw = "",
                 base_url = "https://api.github.com",
                 repo_path = "repos/abc/xyz")

test_that(
  "Single item results are wrapped as list of lists",
  with_mock( "gh::gh" = function(...) list(a = 1L, b = 2L, c = 3L), {
    expect_type(gh::gh(), "list")
    expect_type(gh::gh()[[1]], "integer")
    expect_type(projmgr:::get_engine("issues/", repo_ref), "list")
    expect_type(projmgr:::get_engine("issues/", repo_ref)[[1]], "list")
  } )
)

test_that(
  "Empty results are wrapped as empty character",
  with_mock( "gh::gh" = function(...) structure("", class = c("gh_response", "list")), {
    expect_type(gh::gh(), "character")
    expect_equivalent(gh::gh(), "")
    expect_type(projmgr:::get_engine("issues/", repo_ref), "character")
    expect_equivalent(projmgr:::get_engine("issues/", repo_ref), "")
  } )
)


test_that(
  "Repo metadata is added for non-empty results",
  with_mock( "gh::gh" = function(...) list(a = 1L, b = 2L, c = 3L), {
    expect_equal(projmgr:::get_engine("issues/", repo_ref)[[1]]$repo_owner, "abc")
    expect_equal(projmgr:::get_engine("issues/", repo_ref)[[1]]$repo_name, "xyz")
  } )
)

test_that(
  "Repo metadata is not added for non-empty results",
  with_mock( "gh::gh" = function(...) structure("", class = c("gh_response", "list")), {
    expect_false("repo_owner" %in% names(projmgr:::get_engine("issues/", repo_ref)[[1]]))
    expect_false("repo_name" %in% names(projmgr:::get_engine("issues/", repo_ref)[[1]]))
  } )
)
