
test_that("xoffset does not affect cursors", {
  
  vdiffr::expect_doppelganger("xoffset does not affect cursors", {
    read_ROBOO(ephysdata::examplefile("OO_GABA")) %>%  head(3) %>% mutate(xoffset=swp.start-swp.start[1]) %>% 
      add_bar("peak",39,100, sweeps="all") %>% #get_xBars()
      #add_bar("fit",110,149,  sweeps=0:2) %>% #get_xBars()
      add_cursor_point(name = "peak", start=39,end=100, fun = min) %>%
      add_cursor_model(name = "exp", start=110,end=149, model_fun_exp) %>% 
      ggsweeps()
  })
  #+ 
  #auto_bars()+
  #scale_y_continuous(expand = expansion(mult=c(0.1 ,0.2)))
   
  read_ROBOO(ephysdata::examplefile("OO_GABA")) %>%  head(3) %>% #mutate(xoffset=swp.start-swp.start[1]) %>% 
    add_bar("peak",39,100, sweeps="all") %>% #get_xBars()
    add_bar("fit",110,149,  sweeps=0:2) %>% #get_xBars()
    add_cursor_point(name = "peak", start=39,end=100, fun = min) %>%
    add_cursor_model(name = "exp", start=110,end=149, model_fun_exp)  -> DATA2
  
  vdiffr::expect_doppelganger("xoffset does not affect cursors 2", {
    DATA2 %>% ggsweeps(xoffset="realtime", yoffset=333, lazy = F) 
  })
  
  
  vdiffr::expect_doppelganger("xoffset does not affect cursors 3", {
    read_ROBOO(ephysdata::examplefile("OO_GABA")) %>%  head(3) %>% #mutate(xoffset=swp.start-swp.start[1]) %>% 
      add_bar("peak",39,100, sweeps="all") %>% #get_xBars()
      add_bar("fit",110,149,  sweeps=0:2) %>% #get_xBars()
      add_cursor_point(name = "peak", start=39,end=100, fun = min) %>%
      add_cursor_model(name = "exp", start=110,end=149, model_fun_exp) %>% 
      ggsweeps(xoffset=100, yoffset=333) #+ 
  })
  
  
  
  
})



