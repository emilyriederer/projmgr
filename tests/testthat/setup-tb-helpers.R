df_issue <-
  structure(list(number = 1:3,
                 labels_name = list(character(0), "a", list("a", "b")),
                 assignees_login = list(character(0), "a", list("a", "b")),
                 milestone_number = list(1, NA, 2),
                 created_at = list("2019-01-01", "2019-06-01", "2019-12-31") ),
            row.names = c(NA, -3L),
            class = "data.frame",
            .Names = c("number", "labels_name", "assignees_login",
                       "milestone_number", "created_at"))

df_milestone <-
  structure(list(number = 1:3,
                 n_closed_issues = c(4, 2, 0),
                 due_on = c(NA, NA, "2019-12-31")),
            row.names = c(NA, -3L),
            class = "data.frame",
            .Names = c("number", "n_closed_issues", "due_on"))

df_events <-
  structure(list(number = c(1,1,1,2,2,3),
                 event = c("referenced", "referenced", "a", "b", "referenced","c", "d")),
            row.names = c(NA, -5L),
            class = "data.frame",
            .Names = c("number", "event"))
