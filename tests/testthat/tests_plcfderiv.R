context("plcfderiv")

test_that("test the default method, which just throws an error",{
  object<-list("test","this","is")
  expect_error(plcfderiv(object),"Error in plcfderiv: method not defined for this class")
})

test_that("test that it gives the right format of output",{
  x<-c(1,2,4,12,13)
  y<-c(0,1,-1,100,5)
  obj<-plcf(x,y)
  res<-plcfderiv(obj)
  expect_s3_class(res,"lcf")
})

test_that("test that it gives quantitatively correct output",{
  x<-c(1,2,4,12,13)
  y<-c(0,1,-1,100,5)
  obj<-plcf(x,y)
  res<-plcfderiv(obj)
  expect_equal(res$bpts,x)
  expect_equal(res$vals,c(1,-1,101/8,-95))
})