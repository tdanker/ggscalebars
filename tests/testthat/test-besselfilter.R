test_that("bessel filter works same way from file or string", {
  
  x=c(rep(100,100))
  tol=0.0001
  expect_snapshot_value(bf0.001(x),style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.002(x),style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.005(x),style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.01(x) ,style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.02(x) ,style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.05(x) ,style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.1(x)  ,style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.2(x)  ,style = "json2",tolerance = tol)
  expect_snapshot_value(bf0.5(x)  ,style = "json2",tolerance = tol)

 
})
