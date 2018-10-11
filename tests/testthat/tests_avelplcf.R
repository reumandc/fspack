context("avelplcf")

test_that("test a simple case",{
  bpts<-c(0,1,2)
  bptvals<-c(0,1,0)
  obj<-plcf(bpts,bptvals)
  res<-avelplcf(obj,0.5)
  
  handbpts<-c(0.25,0.75,1.25,1.75)
  handbptvals<-c(1,1,-1,-1)
  expect_equal(res$bpts,handbpts)
  expect_equal(res$bptvals,handbptvals)
})
