ephys4
================


# ephys4 <img src="man/figures/logo.png" align="right" />

## Read, plot and analyse ephys data

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge//lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

The ephys4 package unifies reading HEKA, Roboocyte, and Hamamatsu files
and performs analyses on them.

### Package Installation

If your system is [setup to use this repository](../repo_setup.html),
you can install the package simply by typing:

``` r
install.packages("ephys4")
```

``` r
# read and plot HEKA patch-clamp file
HEKA <- (
  read_PATCHMASTER("VG_Blocker.dat", exp = 1, ser = 2) %>% filter(swp==6) %>%
            add_cursor_point(name = "peak", start=.01,end=.012, fun = min) -> HEKA_results ) %>%
            ggsweeps() + 
            ggtitle("HEKA", subtitle = "Sodium Channel")

# read and plot HAMAMATSU file
HAMA <- (
  
  read_HAMAMATSU("HAMAMATSU/cardiomyocytes.TXT") %>%  filter(well=="A2") %>% 
            add_cursor_points("peak",start=20,end=40, fun = max)     -> HAMA_results) %>%
  
            ggsweeps() + 
            ggtitle("HAMAMTSU", subtitle = "Cardiomyocytes")

# read, analyse, and plot Roboocyte file
ROBO <- (read_ROBOO("roboocyte/0626.1.r2d") %>%  head(1) %>%  
           add_cursor_point(name = "peak", start=39, end=119, fun = min)-> ROBO_results) %>% 
           ggsweeps() + 
           ggtitle("ROBOCYTE", subtitle = "GABA-A")

# show all 3 plots
HEKA + ROBO / HAMA 
```

![](man/figures/readme_plot-1.png)<!-- -->

![](man/figures/drc-1.png)<!-- -->
