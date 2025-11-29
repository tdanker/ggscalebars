test_that("bars are working with NSE (label, start, end, fill)", {
  oo_export<-examplefile("roboocyte/17-05-23.NR1_2A.P2.G1.dat")
  read_Roboocyte_exported_oocyte_dat(oo_export) %>% 
    add_cursor_point("peak", 40, 60, min) -> TMP
  
  swpcolors=c(
    rep("skyblue", 5), 
    heat.colors(10, rev=T),
    rep("skyblue", 5)
  )
  
  # the tricky thing here are the parameters 'border' and 'label.x', if not set manually, because they have 'calculated defaults'. 
  # so it is important that this test is not setting them 
  TMP %>%	
    mutate(bcolor_=swpcolors[as.numeric(swp)]) %>% 
    mutate(start_=0, end_=180) %>% 
    mutate(label_=c("c", "c","c", "c","c", 1:10, "c", "c","c", "c","c" )[as.numeric(swp)]) %>%
    add_bar("c1", label = label_, start_, end_, fill = bcolor_) %>%
    ggsweeps(xoffset="realtime") + theme_classic() + theme(legend.position = "none") + 
    ggtitle("The famous Lundbeck NMDA Assay") -> p
  
  vdiffr::expect_doppelganger("lundbeck plot (bars with NSE)", p)
})