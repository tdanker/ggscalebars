# we seem to have no good example data to test add_r2d=TRUE
# (would require logs written according to our internal standards for marking recordings. )

test_that("logs_per_sweep works", {
  
  logs=logs_per_sweep(
    ephysdata::examplefile("OO_r2d") %>% stringr::str_remove(".r2d"), add_r2d = FALSE, recording.pattern = "==="
    )
  expect_s3_class(logs, "tbl")
  expect_named(logs, c('plate', 'run', 'swp', 'OO', 'loglines', 'run_start', 'run_end'))

})


