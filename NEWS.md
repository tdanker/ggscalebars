# ephys4 4.0.2

This release mainly fixes tests and documentation issues. 

### improvements: 
- geom_topbar can now draw lines to the data. 

### removed functions:
- everything related to reading roboocyte logs in a project specifix way. This will go to another package. 
- all 'accumulator' functions: - we will use the 'targets' library from now on. 

# ephys4 4.0.1

### new features:

-   conditional cursors: you can now say e.g. 'condition=swp==4' to evaluate the cursor only for swp #4
-   cursor_points: long format now plots efficently in ggsweeps
-   streams: now plot nicely with ggsweeps

### minor fixes:

-   improved testthat tests
-   no more 'size is deprecated' warnings in geom_topbar\_ and scalebars.
-   make_ephysdata now throws error when 'id' is malformed

# ephys4 4.0.0

-   First Version of ephys4, derived from ephys3.
