# Todos ----------
# name "add_trace"

# _______________________================
# This package ------
## reader functions ------
export(read_HAMAMATSU)
export(read_PATCHMASTER)
export(read_ROBOO)
export(read_Roboocyte_exported_oocyte_dat)
export(make_ephysdata) # name


## add_trace functions-------
export(add_trace) #name
export(add_trace_) #dont export? why useful?

### filter functions-------
export(bf0.001) # where is the starter-function gone?
export(bf0.002)
export(bf0.005)
export(bf0.01)
export(bf0.02)
export(bf0.05)
export(bf0.1)
export(bf0.2)
export(bf0.5)
export(createFilter_driftcorrection)
export(unfiltered)

## trace math functions ------
export(subtract_traces) # name?

## add_Cursor functions------
export(add_cursor)   #dont export?
export(add_cursor_bar) #?
export(add_cursor_expfit)
export(add_cursor_level)
export(add_cursor_model)
 export(model_fun_exp)
 export(model_fun_exp2)
 export(model_fun_lm)
export(add_cursor_point)
export(add_cursor_point_fast) #?
export(add_cursor_points)









## bars--------
export(add_bar)
export(geom_topbar)



## ggswepps (autoplot_functions)-------
export(ggsweeps)




## annotations ----------

### example for a user level annotation function: ------
export(point_annotation)

### ?------
export(annot_range_model)
export(annot_range_point)


### auto-annotation?------
export(bar_.annot)
export(level_.annot)
export(model_.annot)
export(peaks_multy_.annot)
export(point_.annot)
export(point_.annot_simple)

### annotation geoms ------
export(geom_cursor_AP_)
export(geom_cursor_hline_)
export(geom_cursor_hline_extended)
export(geom_cursor_label_)
export(geom_cursor_model_predict)
export(geom_cursor_point_)
export(geom_cursor_range_)
export(geom_cursor_toplabel_)
export(geom_cursor_xbar_)


## options control ------
export(set_file_searchfolder)


## *Roboocyte_functions (useful but very specific)--------
# they should have "Roboocyte" in theyr name
# they are not exaclty read_ functions
export(get_injection_info)
export(get_recordinginfos)


## *special -------
export(analyse_ROBOO_cached) #still useful? (targets!)

## *Helper files we may want to get rid of ----
export(ephysdata_examplefile)

## *dont export? (undecided) ------
export(format_roboExport) 
export(geom_trace)
export(get_trace)


## **ditch------
export(list_HEKA_files) # why only HEKAfiles?

# _______________________==================
# Separate package with reverse-depends ephys4 ? -------
## scalebars ?------
export(scalebars)
export(theme_scalebar_h)
export(theme_scalebar_v)
export(theme_scalebars)
export(coord_scalebars)

## drc ?--------
export(drc_fit)
export(drc_get_lpresults_)
export(drc_kable)
export(drc_plan_HEKA)
export(drc_plan_manual)
export(drc_plot)
export(drc_table)




## ephys4.HEKA.specials ?------
export(write_explabel)
export(write_exptext)
export(write_serlabel)
export(write_swplabel)

