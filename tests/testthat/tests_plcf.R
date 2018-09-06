context("plcf")

#test_that("test the error handling of plcf"),{
#})

test_that("test the plcf creator function for format of output", {
  x<-1:5
  y<-c(.1,5,-3,4,10)
  res<-plcf(x,y)
  expect_s3_class(res,"plcf")
  expect_s3_class(res,"list")
})

test_that("test the plcf creator function for accuracy of output", {
  x<-1:5
  y<-c(.1,5,-3,4,10)
  res<-plcf(x,y)
  expect_equal(res$bpts,x)
  expect_equal(res$bptvals,y)
})
