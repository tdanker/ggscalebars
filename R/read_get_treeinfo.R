


# slim=F switches on verbose mode for add_atributess. This means sweep and trace sweeplabels
# are included, so that extracted deeper nodes can be plotted etc (untested).
# anymore with slim=T

#' get_treeinfo
#' 
#' Read the tree structure (aka table-of-contents) of a HEKA Patchmaster (.dat) file. 
#' The resulting treeinfo object can subsequently be used to obtain relevant parts of the data 
# 
#' 
#' 
#' @param filename the file to be read
#' 
#' @return An object of class "HEKA_treeinfo"

get_treeinfo <- function(filename) {
  
  con = file(filename, "rb")
  signature<-readChar(con,4)
  if(!(signature=="DAT1" || signature=="DAT2"))
    stop("file type is not supported")

  if(signature=="DAT1"){
    mypul<-stringr::str_replace(filename, ".dat$", ".pul")
    if(!file.exists(mypul)){
      close(con)
      stop("while trying to read from *.dat file, detected the old 'DAT1' format (no bundle file), therefore expected but could not find a correspoding *.pul file"   )
      
    }
          con_pul<- file(mypul, "rb")

    mypgf<-stringr::str_replace(filename, ".dat$", ".pgf")
    if(!file.exists(mypul)){
      close(con)
      stop("while trying to read from *.dat file, detected the old 'DAT1' format (no bundle file), therefore expected but could not find a correspoding *.pgf file"   )
      
    }
          con_pgf<- file(mypgf, "rb")
    
  }else{
    con_pul=con
    con_pgf=con
  }
  
  treeinfo <- list(root = read.bundletree(filename, ".pul", con=con_pul))
  attr(treeinfo[["root"]], "filename") <- filename
  names(treeinfo) <- filename
  class(treeinfo) <- "HEKA_treeinfo"
  
  
  # add path and class attributes, and (unique) names: 
  treeinfo <- add_atributes(treeinfo, con_pul, verbose = TRUE, filename = filename, con_pgf=con_pgf)
  
  if(signature=="DAT1"){  
    close(con_pul)
    close(con_pgf)
  }
  
  close(con)
  treeinfo
}




# add attributes for names, path, and class
# -make all names of the tree unique (for series and sweeps read label to make a name)
# -add classes to each level
# -add path to series (and optionaly deeper) nodes
add_atributes <- function(tree, con, verbose = F, filename, con_pgf=con) {
  bundletree.pgf <- read.bundletree(filename, ".pgf", con_pgf)  
  for (rootname in names(tree)) {
        # add class
        attr(tree[[c(rootname)]], "class") <- c("HEKA_treeinfo_rootnode", "HEKA_treeinfo")
        
        # make unique experiment names
        GRLabels <- lapply(tree[[c(rootname)]], function(item) {
          readlabel(attr(item, "dataptr"), con)
        })
        
        # the next two could also be used to make the expname in the tree:
        
        # GrExperimentNumbers <- lapply(tree[[c(rootname, exp)]], function(item) {
        #   readAny(attr(item, "dataptr"), con, 116, "int", 4)
        # })
        # 
        # Groupcounts <- lapply(tree[[c(rootname, exp)]], function(item) {
        #   readAny(attr(item, "dataptr"), con, 120, "int", 4)
        # })
        
        attr(tree[[c(rootname)]], "names") <- paste(1:length(tree[[c(rootname)]]), ":", GRLabels)
        
        
        
        
        for (exp in names(tree[[rootname]])) {
            # add class 
            attr(tree[[c(rootname, exp)]], "class") <- c("HEKA_treeinfo_experimentnode", "HEKA_treeinfo")          
          
            # make unique series names
            sernames <- lapply(tree[[c(rootname, exp)]], function(item) {
              readlabel(attr(item, "dataptr"), con)
            })
            
            
            attr(tree[[c(rootname, exp)]], "names") <- paste(1:length(tree[[c(rootname, exp)]]), 
                ":", sernames)
            
            # store the experiment label in an optional attribute
            label     <- readlabel( attr(tree[[c(rootname, exp)]], "dataptr"), con)
            expnumber <- readAny(   attr(tree[[c(rootname, exp)]], "dataptr"), con, 116, "int", 4)
            
            if(!label==paste0("E-",expnumber)){
              attr(tree[[c(rootname, exp)]], "ExperimentLabel") <- label
            }

            
            for (ser in names(tree[[c(rootname, exp)]])) {
                # add class
                attr(tree[[c(rootname, exp, ser)]], "class") <- c("HEKA_treeinfo_seriesnode", "HEKA_treeinfo")   
                
                # add path
                attr(tree[[c(rootname, exp, ser)]], "path") <- c(rootname, exp, ser)

                # make unique sweep names
                sweeplabels = lapply(tree[[c(rootname, exp, ser)]], function(item) {
                  readlabel(attr(item, "dataptr"), con)
                })
                attr(tree[[c(rootname, exp, ser)]], "names") <- paste("s", 1:length(tree[[c(rootname, 
                  exp, ser)]]), ":", sweeplabels, sep = "")
                
                # read *real* stimulus name from file (not from label)
                stimcount <-
                  readAny(attr(tree[[c(rootname, exp, ser)]][[1]], "dataptr"), con, 40, "int", 2)
                StimName <- 
                  readAny(attr(bundletree.pgf[[stimcount]], "dataptr"), con_pgf, 4, "char", NA)
                attr(tree[[c(rootname, exp, ser)]], "StimulusName") <- StimName
                
                # store the series label in an optional attribute
                label=readlabel(attr(tree[[c(rootname, exp, ser)]], "dataptr"), con)
                if(!label==StimName){
                  attr(tree[[c(rootname, exp, ser)]], "SeriesLabel") <- label
                }

                  
                # sweep and trace attributes can be set optionally this enables deeper subsetting but makes
                # the tree much larger (is this really still optional?)
                if (verbose) {
                  for (sweep in names(tree[[c(rootname, exp, ser)]])) {
                    attr(tree[[c(rootname, exp, ser, sweep)]], "path") <- c(rootname, exp, 
                      ser, sweep)
                    attr(tree[[c(rootname, exp, ser, sweep)]], "class") <- c("HEKA_treeinfo_sweepnode", 
                      "HEKA_treeinfo")
                    attr(tree[[c(rootname, exp, ser, sweep)]], "names") <- lapply(tree[[c(rootname, 
                      exp, ser, sweep)]], function(trace) {
                      readlabel(attr(trace, "dataptr"), con)
                    })
                    # attr(tree[[c(rootname,exp,ser,sweep)]],'names')<-paste('trace
                    # ',1:length(tree[[c(rootname,exp,ser,sweep)]]))
                    for (trace in names(tree[[c(rootname, exp, ser, sweep)]])) {
                      attr(tree[[c(rootname, exp, ser, sweep, trace)]], "path") <- c(rootname, 
                        exp, ser, sweep, trace)
                      attr(tree[[c(rootname, exp, ser, sweep, trace)]], "class") <- c("HEKA_treeinfo_tracenode", 
                        "HEKA_treeinfo")
                    }
                  }
                }
            }
        }
    }
    tree
}
