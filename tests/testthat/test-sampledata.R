test_that ("sample_data", {
               dat <- generate_data (1, 2, 1)
               testthat::expect_type (dat, "list")

               testthat::expect_error (
                   generate_data (0, 1, 1),
                   "layers has to be >= 1")
               testthat::expect_error (
                   generate_data (1, 1, 1),
                   "n_objects has to be > 1")
               testthat::expect_error (
                   generate_data (1, 2, 2),
                   "change_rate has to be between 0 and 1.")
})
