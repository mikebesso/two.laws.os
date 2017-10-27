library(testthat)
library(two.laws.os)

OS <- two.laws.os:::WinOS$new()

x <- OS$GetDateFormat()

context("WinOS")

test_that(
  "AssertStandardDateFormat",
  {
    OS <- WinOS$new()

    DateFormat <- OS$GetDateFormat()

    if (DateFormat != "yyyy-MM-dd"){
      expect_error(OS$AssertStandardDateFormat())
    } else {
      expect_error(OS$AssertStandardDateFormat(), NA)
    }

  }

)
