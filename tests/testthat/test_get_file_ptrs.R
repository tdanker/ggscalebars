test_that("ptrs work from subdirs", {
  

  testdir <- withr::local_tempdir()
  withr::with_dir(testdir, {
    
    file.copy(ephysdata::examplefile("NaV"), to=".")
    ephysdata<-read_PATCHMASTER("VG_Blocker.dat")
    
    # if we include "./" in the searchfolder, we should be able to get the traces: 
    set_file_searchfolder(list("./", "../", "../../"))
    expect_s3_class({
      suppressWarnings( ephysdata %>% head(1) %>% get_trace() ) 
    }, "data.frame")
    
    # create and cd into a subdir
    dir.create("subdir", showWarnings = F)
    setwd("./subdir")
    
    # now, if we unset the searchfolder, we expext an error
    set_file_searchfolder(list())
    expect_error(
      ephysdata %>% head(1) %>% get_trace()  
    )
    
    # but with searchfolder set, it will be found
    expect_s3_class({
      set_file_searchfolder(list("./", "../", "../../"))
      ephysdata %>% head(1) %>% get_trace()  
    }, "data.frame")
    
    # we are still in the subdir, so if we copy our testfile here, it will now be in 2 places,
    # which should produce a warning:
    expect_warning({
      file.copy(ephysdata::examplefile("NaV"), to=".")
      set_file_searchfolder(list("./", "../", "../../"))
      ephysdata %>% head(1) %>% get_trace()
    }, "file was found in more than one place")
  })
  
  
  
})





