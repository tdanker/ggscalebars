test_that("ptrs work from subdirs", {
  skip("copy files introduices problems with other tests, skipping until this is fixed")
  file.copy(ephysdata::examplefile("NaV"), to=".")
  ephysdata<-read_PATCHMASTER("VG_Blocker.dat")
  set_file_searchfolder(list("./", "../", "../../"))
  
  expect_s3_class({
    suppressWarnings( ephysdata %>% head(1) %>% get_trace() ) 
  }, "data.frame")
  
  
  dir.create("subdir", showWarnings = F)
  setwd("./subdir")
  set_file_searchfolder(list())
  expect_error(
    ephysdata %>% head(1) %>% get_trace()  
  )
  
  expect_s3_class({
    set_file_searchfolder(list("./", "../", "../../"))
    ephysdata %>% head(1) %>% get_trace()  
  }, "data.frame")
  
  
  expect_warning({
    file.copy(ephysdata::examplefile("NaV"), to=".")
    set_file_searchfolder(list("./", "../", "../../"))
    ephysdata %>% head(1) %>% get_trace()
    file.remove("VG_Blocker.dat")
  }, "file was found in more than one place")
  
  
})





