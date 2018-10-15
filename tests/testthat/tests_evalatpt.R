context("evalatpt")

test_that("test for lcf object",{
  bpts<-c(0,1,3)
  vals<-c(0,1)
  obj<-lcf(bpts,vals)
  expect_equal(evalatpt(obj,-0.1),NA*numeric(1))
  expect_equal(evalatpt(obj,0),NA*numeric(1))
  expect_equal(evalatpt(obj,.5),0)
  expect_equal(evalatpt(obj,1),NA*numeric(1))
  expect_equal(evalatpt(obj,2),1)
  expect_equal(evalatpt(obj,3),NA*numeric(1))
  expect_equal(evalatpt(obj,3.1),NA*numeric(1))
})

test_that("test for lcf object, vector input",{
  bpts<-c(0,1,3)
  vals<-c(0,1)
  obj<-lcf(bpts,vals)
  expect_equal(evalatpt(obj,c(-0.1,0,.5,1,2,3,3.1)),c(NA,NA,0,NA,1,NA,NA))
})

test_that("test for plcf object",{
  bpts<-c(0,1,2)
  bptvals<-c(0,1,.5)
  obj<-plcf(bpts,bptvals)
  expect_equal(evalatpt(obj,0),0)
  expect_equal(evalatpt(obj,1),1)
  expect_equal(evalatpt(obj,2),.5)
  expect_equal(evalatpt(obj,.5),.5)
  expect_equal(evalatpt(obj,1.5),.75)
  expect_equal(evalatpt(obj,-0.1),NA*numeric(1))
  expect_equal(evalatpt(obj,2.1),NA*numeric(1))
})

test_that("test for plcf object, vector input",{
  bpts<-c(0,1,2)
  bptvals<-c(0,1,.5)
  obj<-plcf(bpts,bptvals)
  expect_equal(evalatpt(obj,c(0,1,2,.5,1.5,-.1,2.1)),c(0,1,.5,.5,.75,NA,NA))  
})

test_that("test the default method",{
  expect_error(evalatpt("test",2),"Error in evalatpt: method not defined for this class")
})
