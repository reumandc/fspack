context("plotplcfs")

test_that("test error handling",{
  expect_error(plotplcfs("test",xlabel="testx",ylabel="testy"),
               "Error in plotplcfs: plcflist must be a list of plcf objects")
  expect_error(plotplcfs(list("test"),xlabel="testx",ylabel="testy"),
               "Error in plotplcfs: plcflist must be a list of plcf objects")
  
  plcf1<-plcf(c(1,2,3,4),c(1,0,1,4))
  plcf2<-plcf(c(1.5,2.5,3.5),c(-1,-2,-5))
  h<-list(plcf1,plcf2)
  expect_error(plotplcfs(h,xlabel="testx",ylabel="testy",filename=1),
               "Error in plotplcfs: inappropriate filename argument")
  expect_error(plotplcfs(h,xlabel="testx",ylabel="testy",bds="test"),
               "Error in plotplcfs: bds must be numeric")
  expect_error(plotplcfs(h,xlabel="testx",ylabel="testy",bds=1),
               "Error in plotplcfs: bds must be length 2")
  expect_error(plotplcfs(h,xlabel="testx",ylabel="testy",bds=c(1,NA)),
               "Error in plotplcfs: bds must have finite elements")
  expect_error(plotplcfs(h,xlabel="testx",ylabel="testy",bds=c(3,2)),
               "Error in plotplcfs: first element of bds must be less than second element")
  expect_error(plotplcfs(h,xlabel=1,ylabel="testy"),
               "Error in plotplcfs: inappropriate xlabel argument")
  expect_error(plotplcfs(h,xlabel="testx",ylabel=c("testy1","testy2")),
               "Error in plotplcfs: inappropriate ylabel argument")
})

test_that("test the output for some simple cases",{
  plcf1<-plcf(c(1,2,3,4),c(1,0,1,5))
  plcf2<-plcf(c(1.5,2.5,3.5),c(-1,-2,-5))
  h<-list(plcf1,plcf2)
  Test_plotplcfs_1<-function(){plotplcfs(plcflist=h[1],xlabel="time (days)",ylabel="river km")}
  expect_doppelganger(title="Test-plotplcfs-1",fig=Test_plotplcfs_1)
  
  Test_plotplcfs_2<-function(){plotplcfs(plcflist=h,xlabel="text x lab",ylabel="text y lab")}
  expect_doppelganger(title="Test-plotplcfs-2",fig=Test_plotplcfs_2)
})