context("LISTCOL functions")

data <- structure(
  list(
    number = 1:3,
    labels = list("q", character(0), c("a:1", "b:2", "c-3"))
  ),
  row.names = c(NA, -3L),
  class = "data.frame",
  .Names = c("number", "labels"))

data2 <- structure(
  list(
    number = 1:3,
    labels = c("a:5", "b:3", "a:1")
  ),
  row.names = c(NA, -3L),
  class = "data.frame",
  .Names = c("number", "labels"))

test_that("Listcol extract funcion creates appropriate new columns",
          {
            expect_named( listcol_extract(data, "labels", "a:"), c("number", "labels", "a"))
            expect_named( listcol_extract(data, "labels", "b:"), c("number", "labels", "b"))
            expect_named( listcol_extract(data, "labels", "c:"), c("number", "labels", "c"))
            expect_named( listcol_extract(data, "labels", "c-"), c("number", "labels", "c"))
            expect_named( listcol_extract(data, "labels", "a:", new_col_name = "q"), c("number", "labels", "q"))
            expect_named( listcol_extract(data, "labels", ":"), c("number", "labels", "V3"))

          })

test_that("listcol_extract function populations columns with correct values",
          {
            expect_equal( listcol_extract(data, "labels", "a:")$a, c(NA, NA, "1"))
            expect_equal( listcol_extract(data, "labels", "a:", keep_regex = TRUE)$a, c(NA, NA, "a:1"))
            expect_equal( listcol_extract(data, "labels", "b:")$b, c(NA, NA, "2"))
            expect_true( all(is.na(listcol_extract(data, "labels", "c:")$c) ))
            expect_equal( listcol_extract(data, "labels", "c-")$c, c(NA, NA, "3"))
            expect_equal( listcol_extract(data, "labels", ":")$V3[[3]], c("a1", "b2"))
          }
)

test_that("listcol_filter preserves the correct values",
          {
            expect_equal( nrow(listcol_filter(data, "labels", matches = "q")), 1)
            expect_equal( nrow(listcol_filter(data, "labels", matches = c("a:", "b:"), is_regex = TRUE)), 1)
            expect_equal( nrow(listcol_filter(data, "labels", matches = c(".*"), is_regex = TRUE)), 2)
            expect_equal( listcol_filter(data, "labels", matches = "q")$number, 1)
            expect_equal( listcol_filter(data, "labels", matches = c("a:", "b:"), is_regex = TRUE)$number, 3)
            expect_equal( nrow(listcol_filter(data2, "labels", matches = c("a:", "b:"), is_regex = TRUE)), 3)
          }
)

test_that("listcol_pivot creates appropriate new columns",
          {
            expect_named(listcol_pivot(data, "labels", "^(a|b):"), c("number", "labels", "a:1", "b:2"))
            expect_named(
              listcol_pivot(data, "labels", "^(a|b):",
                            transform_fx = function(x) gsub("^(a|b):", "", x)),
              c("number", "labels", "1", "2"))

          }
)

