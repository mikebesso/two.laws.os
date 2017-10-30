library(testthat)
library(two.laws.os)


context("WinOS")

test_that(
  "AssertStandardDateFormat",
  {

    skip_on_os(c("mac", "linux", "solaris"))

    DateFormat <- GetOS()$GetDateFormat()

    if (DateFormat != "yyyy-MM-dd"){
      expect_error(GetOS()$AssertStandardDateFormat())
    } else {
      expect_error(GetOS()$AssertStandardDateFormat(), NA)
    }

  }

)
