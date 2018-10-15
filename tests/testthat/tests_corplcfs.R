context("corplcfs")

test_that("test the error handling",{
  plcf1<-plcf(c(0.5,1.5,2.5,3.5,4.5),c(3,4,3,4,3))
  plcf2<-plcf(c(0,1,2,3,4,5),c(1,2,1,2,1,2))
  expect_error(corplcfs(plcf1,plcf2,"test"),
               "Error in corplcfs: bds argument must be numeric or NULL")
  expect_error(corplcfs(plcf1,plcf2,1),
               "Error in corplcfs: bds argument must be a length-2 numeric vector or NULL")
  expect_error(corplcfs(plcf1,plcf2,c(1,5)),
               "Error in corplcfs: bds must not include values for which one or both of obj1 or obj2 is not defined")
  
  plcf1<-plcf(c(0.5,1.5,2.5,3.5,4.5),c(3,4,3,4,3))
  plcf2<-plcf(c(0,1,2,3,4,5)+10,c(1,2,1,2,1,2))
  expect_error(corplcfs(plcf1,plcf2,NULL),
               "Error in corplcfs: obj1 and obj2 not both defined for any range of values")
})

test_that("test the quantitative accuracy of the output",{
  plcf1<-plcf(c(0,1),c(0,1))
  plcf2<-plcf(c(0,1),c(1,2))
  expect_equal(corplcfs(plcf1,plcf2,NULL),1)

  #***DAN: you should probably put in another text or two
})

