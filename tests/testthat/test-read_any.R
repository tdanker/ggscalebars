# read_any
# 
# here, we check promises that we make for reading on all platforms

list.files("cache_robotraces/", full.names = T) %>% unlink
robo.r2d <- read_ROBOO(       ephysdata::examplefile("OO_r2d")  ,get_exported_datatable = FALSE  )
robo <- read_ROBOO(       ephysdata::examplefile("OO_r2d")  ,get_exported_datatable = TRUE  )
heka <- read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2)
hama <- read_HAMAMATSU(   ephysdata::examplefile("HT_cm")     )


x=(1:100)/20
y=sin(x)
any_ <-  make_ephysdata(data.frame(id="test", swp=1, x=x, y=y))

oo_export<-examplefile("roboocyte/17-05-23.NR1_2A.P2.G1.dat")
robo2 <- read_Roboocyte_exported_oocyte_dat(oo_export)

# several types of plots require to group by swp, so this should always be present. 
# we want it to be a factor so that ggplot knows to apply the correct scale (i.e. discrete colors)
test_that("has column swp which is a factor ", {
  expect_s3_class(robo$swp, "factor")
  expect_s3_class(robo.r2d$swp, "factor")
  expect_s3_class(heka$swp, "factor")
  expect_s3_class(hama$swp, "factor")
  expect_s3_class(any_$swp, "factor")
  expect_s3_class(robo2$swp, "factor")
})

test_that("has column swp.start which is of type double ", {
  expect_type(robo$swp.start, "double")
  expect_type(robo.r2d$swp.start, "double")
  expect_type(heka$swp.start, "double")
  expect_type(hama$swp.start, "double")
  expect_type(any_$swp.start, "double")
  expect_type(robo2$swp.start, "double")
})




# we should keep grouping of the output of read_xx consistent and stable
# to ensure that in user land, operations on these tibbles, i.e.  manipulations to xoffset,
# do not change their behavior.

test_that("no grouping is present in the output of read_xxx", {
  expect_true(length(group_vars(robo))==0)
  expect_true(length(group_vars(heka))==0)
  expect_true(length(group_vars(hama))==0)
  expect_true(length(group_vars(any_))==0)
  expect_true(length(group_vars(robo2))==0)
})

test_that("no 'rowwise' is set in the output of read_xxx", {
  expect_true(n_groups(robo) == 1)
  expect_true(n_groups(heka) == 1)
  expect_true(n_groups(hama) == 1)
  expect_true(n_groups(any_) == 1)
  expect_true(n_groups(robo2) == 1)
})
