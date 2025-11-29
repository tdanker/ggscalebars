.onLoad <- function(libname, pkgname) {
  options(ephys4.HEKA_ptrs_as_lists  = TRUE)
  options(ephys4.ROBOO_ptrs_as_lists = TRUE)
  options(ephys4.HAMA_ptrs_as_lists  = TRUE)
  options("ephys4.cursor_annots_not_from_attr"=TRUE)
}
