 Performance - ToDos
 
 [x] avoid using lists for ptrs also in ROBO and HAMA
 
 
        
 [x] implement all relevant parameters of get_trace also in get_trace___fast
   [x] may be ditch filter_fun_2, it is used only in vignette subtract_traces, und is not clear what it is needed for even there? 
      [x] it operates on the complete row - so it cannot be performant in get_traces. should go away here. nobody needs it. 
      
 
 
 [x] implement good tests for get_trace
 
 # CRITICAL
 
 - add_cursors still uses get.ephysdata_using_dataPronoun
 - add stream da passen die Namen noch nicht
 [x] ggsweeps plottet die Curor-annots nicht, abhängig vom Stream-usage? (-> streams are now required)
 - ggsweeps stellt streams nicht vom grouping her korrekt dar (wegen der fehlenden Namen?)
 - ggsweeps gehört kräftig aufgeräumt....
 
 # ------------------------------------------------------------------------------------
 
 - try to avoid cut-df and downsample_df, do it in the methods of get_trace___fast instead. should be more performant (test this) 
 
 - avoid calling xfun::cache_rds if caching is turned off, esp. for traces (in ROBOOCYTE!)   
      -it consumes performance even with "rerun=FALSE", since a file has to be read, and is written if not present, etc...
        - get.ROBOOCYTE       maybe not good to cache here ?
        - get.ROBOOCYTE_      has a comment that caching works good
        - read_ROBOO_r2d      this is about wellinfo, not the traces. so maybe makes sense
        -"add_r2d"            seems to have its own cache. if it operates on many traces at once, it may be fine
        - analyse_ROBOO_cached, is this still needed? (sounds desperate)
        
 
 - finally, switch to get_trace___fast, run all tests
 
 
 - maybe make it faster by sharing the connection. 
