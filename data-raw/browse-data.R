# url mapping
urls <- tibble::tribble(
  ~action, ~object, ~url_suffix,
  'get', 'milestone', "issues/milestones/#list-milestones-for-a-repository",
  'get', 'issue', "issues/#list-issues-for-a-repository",
  'get', 'issue event', "issues/events/#list-events-for-an-issue",
  'get', 'repo labels', 'issues/labels/#list-all-labels-for-this-repository',
  'get', 'issue comment', 'issues/comments/#list-comments-on-an-issue',
  'post', 'milestone', "issues/milestones/#create-a-milestone",
  'post', 'issue', "issues/#create-an-issue",
  'post', 'issue event', NA_character_,
  'post', 'repo labels', 'issues/labels/#create-a-label',
  'post', 'issue comment', 'issues/comments/#create-a-comment'
)

browse_data <- as.data.frame( urls )
usethis::use_data(browse_data, internal = TRUE)
