context("errcheckplcfs")

test_that("test the case of no problem",{
  bpts<-c(0,10,20,30,40) #these are times
  bptvals<-c(300,290,290,200,100) #river kms
  obj<-plcf(bpts,bptvals)
  reldatetime<-0
  upstreamthresh<-5
  timethresh<-30
  distthresh<-100
  expect_equal(errcheckplcf(obj,reldatetime,upstreamthresh,timethresh,distthresh),"N")
})

test_that("test the case of detections before the release",{
  bpts<-c(0,10,20,30,40) #these are times
  bptvals<-c(300,290,290,200,100) #river kms
  obj<-plcf(bpts,bptvals)
  reldatetime<-5
  upstreamthresh<-5
  timethresh<-30
  distthresh<-100
  expect_equal(errcheckplcf(obj,reldatetime,upstreamthresh,timethresh,distthresh),"B")
})

test_that("test the case of not being detected for too long and too far",{
  bpts<-c(0,30,31,32,40) #these are times
  bptvals<-c(300,190,190,180,150) #river kms
  obj<-plcf(bpts,bptvals)
  reldatetime<-0
  upstreamthresh<-5
  timethresh<-25
  distthresh<-100
  expect_equal(errcheckplcf(obj,reldatetime,upstreamthresh,timethresh,distthresh),"T")
})

test_that("test the case of going upsteam too far",{
  bpts<-c(0,10,20,30,40) #these are times
  bptvals<-c(300,290,295,301,100) #river kms
  obj<-plcf(bpts,bptvals)
  reldatetime<-0
  upstreamthresh<-10
  timethresh<-30
  distthresh<-100
  expect_equal(errcheckplcf(obj,reldatetime,upstreamthresh,timethresh,distthresh),"U")
})

test_that("test some mixed cases where more than one things is wrong",{
  bpts<-c(0,30,32,38,40) #these are times
  bptvals<-c(300,190,195,202,100) #river kms
  obj<-plcf(bpts,bptvals)
  reldatetime<-1
  upstreamthresh<-10
  timethresh<-25
  distthresh<-100
  expect_equal(errcheckplcf(obj,reldatetime,upstreamthresh,timethresh,distthresh),"BUT")
})

