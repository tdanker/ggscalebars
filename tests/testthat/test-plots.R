# Plots from readme_more.Rmd
options("ephys4.cursor_annots_not_from_attr"=TRUE)

test_that("we can do all Plots from Readme_more.Rmd", {
  suppressWarnings(library(vdiffr))
  suppressWarnings(library(ephysdata))
  
  #xfun::proj_root()# proj
  
  list.files("cache_robotraces/", full.names = T) %>% unlink
data.<-
  read_PATCHMASTER(ephysdata::examplefile("NaV"), ser=2) %>% tail(3)

expect_doppelganger("simple",{
  data. %>% ggsweeps(start=NA, end=NA, mapping=aes(color=swp), maxpoints =1e17) + xlim(0.008,0.02)
})

expect_doppelganger("custom_annot",{
  showpoint<-function(color) function(name){ point_.annot(name, color)}
  sp2 <-function(name) geom_cursor_max_(name, shape=1, size=3, stroke=2, color="red")
  noannot = function(name){NULL}
  mapped= function(name) geom_cursor_point_(name, shape=1, size=3, stroke=2, mapping=aes(color=swp))
  
  data. %>%
    add_cursor_point("peak2", start=0.028, end=0.033, min, annot = showpoint("orange"))   %>% 
    add_cursor_point("peak3", start=0.009, end=0.013, min, annot=  mapped)  %>% 
    add_cursor_point("peak" , start=0.009, end=0.013, min)  %>% 
    ggsweeps(mapping=aes(color=swp))
})

expect_doppelganger("model_cursors",{
  data. %>% filter(swp %in% c(10,12)) %>%  
    add_cursor_model("lm" , 0.001, 0.009, model_fun_lm, st2=0.01, en2=0.03) %>%
    add_cursor_model("exp", 0.011, 0.019, model_fun_exp, st2=0.01, en2=0.03) %>%
    
    ggsweeps(yoffset = .2, xoffset=0.02) + 
    geom_cursor_model_predict("lm", mapping=aes(x,y,color=paste(id, "lm")))+
    geom_cursor_model_predict("exp", mapping=aes(x,y,color=paste(id, "exp"))) + 
    scale_color_manual(values=c("red", "orange", "blue", "skyblue"))
})


expect_doppelganger("model_cursors2",{
  data. %>% filter(swp ==10) %>%  
    add_cursor_model("exp" , 0.001, 0.0095, model_fun_exp, st2=0.01, en2=0.03) %>%
    add_cursor_model("lm" , 0.02, 0.03, model_fun_lm, st2=0.003, en2=0.03)  %>%
    ggsweeps
})





expect_doppelganger("level cursor",{
  data. %>% head(2) %>%  
    add_cursor_level("lvl", 0.003, 0.009, median) %>%
    ggsweeps()
})


expect_doppelganger("custom geom_cursor_x",{
  data. %>% tail(3) %>%  
    add_cursor_point("peak" , 0.009, 0.013, min)  %>%                       
    
    ggplot(aes(x,y, group=id)) +                                              
    geom_cursor_range_("peak", col = alpha("black", 0.2)) + 
    geom_cursor_point_("peak", shape=1, size=3, stroke=2, color="grey75") +
    geom_cursor_point_("peak", size=1, color="green") +
    
    geom_trace(start=0.005, end=0.02) +                       
    theme_classic() +facet_wrap(~swp)
})
 




expect_doppelganger("hama_peak_base_exp",{
  
  read_HAMAMATSU(ephysdata::examplefile("HT_cm")) %>%  filter(well=="A2") %>% mutate(x1=7.3, x2=10) %>%
    add_cursor("peak", x1, x2, point_,  fun=max) %>%
    add_cursor("base", x1, x2, point_,  fun=min) %>%
    add_cursor("exp", peak.csr$x, base.csr$x, model_,  model_fun=model_fun_exp) %>%
    ggsweeps() + facet_wrap(~well) + xlim(0,11)
  
})




expect_doppelganger("auto bars",{
  read_ROBOO(ephysdata::examplefile("OO_GABA")) %>%  head(3) %>%   
    add_bar(name = "GABA",                        start=35,end=95, line=2, sweeps=list(0,1,2)) %>%
    add_bar(name = "EFX",                         start=00,end=95, line=1, sweeps=1          ) %>%
    add_bar(name = "EFX2",  label="", fill="orange", start=0, end=20, line=1, sweeps=list(0,2  )) %>%
    add_cursor_point("peak", 75,95,min) %>%
    
    ggsweeps + 
    #auto_bars(space = 0.005, height = 0.05) +
    facet_wrap(~swp)  + 
    scale_y_continuous(expand = expansion(mult = c(0.01, .11))) + 
    theme_void()  + 
    theme(strip.text = element_blank(), legend.position = "none")
  
})

expect_doppelganger("auto bars 2",{
  read_ROBOO(ephysdata::examplefile("OO_GABA")) %>%  head(3) %>%   
    add_bar(name = "GABA",  label="", fill="orange",start=35,end=95, line=2, sweeps=list(0,1,2)) %>%
    add_bar(name = "EFX",   label="", fill="grey", start=00,end=95, line=1, sweeps=1          ) %>%
    add_bar(name = "EFX2",  label="", fill="skyblue", start=0, end=20, line=1, sweeps=list(0,2  )) %>%
    add_cursor_point("peak", 75,95,min) %>%
    
    ggsweeps + 
    
    facet_wrap(~swp)  + 
    scale_y_continuous(expand = expansion(mult = c(0.03, .11))) + 
    theme_void()  + 
    theme(strip.text = element_blank(), 
          #legend.position.inside =  c(1,0), 
          legend.justification=c(1,0),
          legend.key.height = unit(.6,"mm"),
          legend.direction = "horizontal",
          legend.background = element_rect(fill="white", color="white", linewidth =6),
          legend.title = element_blank()
    )
})
  
})





  






