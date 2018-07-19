library(testthat)
library(FARSFunctions)

context("Test FARS functions")

test_that("Test fars_read()",{
      df <- fars_read("accident_2015.csv.bz2")
      expect_that(df,is_a("tbl_df"))
      expect_that(fars_read("accident_2018.csv.bz2"),
                  throws_error("file 'accident_2018.csv.bz2' does not exist"))
})

test_that("Test make_filename()",{
      c <- make_filename(2015)
      expect_equal(c,"accident_2015.csv.bz2")
})

test_that("Test fars_read_years()",{
      df <- fars_read_years(2015)
      expect_that(df,is_a("list"))
      expect_that(fars_read_years(2018),
                  gives_warning("invalid year: 2018"))
})

test_that("Test fars_summarize_years()",{
      df <- fars_summarize_years(c(2013,2014,2015))
      expect_that(df,is_a("tbl_df"))
      expect_that(fars_summarize_years(c(2013,2018)),
                  gives_warning("invalid year: 2018"))
})

test_that("Test fars_map_state()",{
      m <- fars_map_state(1,2015)
      expect_equal(m,NULL)
      expect_that(fars_map_state(100,2015),
                  throws_error("invalid STATE number: 100"))
})
