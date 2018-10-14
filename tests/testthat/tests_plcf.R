context("plcf")

#***DAN: this is incomplete
test_that("test the error handling of plcf",{
  x<-c(1,2,2,3,4)
  y<-c(1,2,3,4,5)
  expect_error(plcf(x,y),"Error in plcf: repeats in bpts without coincident repeats in bptsvals")
})

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
  
  x<-c(1,1,2,2,3,3,4)
  y<-c(2,2,1,1,4,4,1)
  res<-plcf(x,y)
  expect_equal(res$bpts,c(1,2,3,4))
  expect_equal(res$bptvals,c(2,1,4,1))
})
