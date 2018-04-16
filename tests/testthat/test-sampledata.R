test_that ("sample_data", {
               dat <- generate_data (1)
               testthat::expect_type (dat, "list")
})
