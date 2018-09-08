context("plotlcfs")

test_that("test error handling",{
  expect_error(plotlcfs("test",xlabel="testx",ylabel="testy"),
               "Error in plotlcfs: lcflist must be a list of lcf objects")
  expect_error(plotlcfs(list("test"),xlabel="testx",ylabel="testy"),
               "Error in plotlcfs: lcflist must be a list of lcf objects")
  
  lcf1<-lcf(c(1,2,3,4),c(1,0,1))
  lcf2<-lcf(c(1.5,2.5,3.5),c(-1,-2))
  h<-list(lcf1,lcf2)
  expect_error(plotlcfs(h,xlabel="testx",ylabel="testy",filename=1),
               "Error in plotlcfs: inappropriate filename argument")
  expect_error(plotlcfs(h,xlabel="testx",ylabel="testy",bds="test"),
               "Error in plotlcfs: bds must be numeric")
  expect_error(plotlcfs(h,xlabel="testx",ylabel="testy",bds=1),
               "Error in plotlcfs: bds must be length 2")
  expect_error(plotlcfs(h,xlabel="testx",ylabel="testy",bds=c(1,NA)),
               "Error in plotlcfs: bds must have finite elements")
  expect_error(plotlcfs(h,xlabel="testx",ylabel="testy",bds=c(3,2)),
               "Error in plotlcfs: first element of bds must be less than second element")
  expect_error(plotlcfs(h,xlabel=1,ylabel="testy"),
               "Error in plotlcfs: inappropriate xlabel argument")
  expect_error(plotlcfs(h,xlabel="testx",ylabel=c("testy1","testy2")),
               "Error in plotlcfs: inappropriate ylabel argument")
})

test_that("test the output for some simple cases",{
  lcf1<-lcf(c(1,2,3,4),c(1,0,1))
  lcf2<-lcf(c(1.5,2.5,3.5),c(-1,-2))
  h<-list(lcf1,lcf2)
  Test_plotlcfs_1<-function(){plotlcfs(lcflist=h[1],xlabel="time (days)",ylabel="river km")}
  expect_doppelganger(title="Test-plotlcfs-1",fig=Test_plotlcfs_1)
  
  Test_plotlcfs_2<-function(){plotlcfs(lcflist=h,xlabel="text x lab",ylabel="text y lab")}
  expect_doppelganger(title="Test-plotlcfs-2",fig=Test_plotlcfs_2)
})