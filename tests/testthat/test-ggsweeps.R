test_that(" a simple ggsweeps works", {
  expect_no_error(
    read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) %>% head(3)  %>% ggsweeps()
  )
  
  
  
})
