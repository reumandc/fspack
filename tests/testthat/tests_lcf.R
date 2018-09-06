context("lcf")

#test_that("test the error handling of lcf"),{
#})

test_that("test the lcf creator function for format of output", {
  x<-1:5
  y<-c(.1,5,-3,4)
  res<-lcf(x,y)
  expect_s3_class(res,"lcf")
  expect_s3_class(res,"list")
})

test_that("test the lcf creator function for accuracy of output", {
  x<-1:5
  y<-c(.1,5,-3,4)
  res<-lcf(x,y)
  expect_equal(res$bpts,x)
  expect_equal(res$vals,y)
})
