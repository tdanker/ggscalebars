test_that("custom_cursor annotations work", {
  expect_no_error({
    read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) %>% slice(6) %>%
      add_cursor_point("test", start = 0.01, end = 0.013, fun=min, 
                       annot = point_annotation( 
                         point.color="orange", 
                         range.color = "grey")
                       )	%>% ggsweeps()
  })
  
})
