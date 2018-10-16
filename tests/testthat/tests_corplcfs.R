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

  plcf1<-plcf(c(0,1),c(0,1))
  plcf2<-plcf(c(0,1),c(2,1))
  expect_equal(corplcfs(plcf1,plcf2,NULL),-1)

  plcf1<-plcf(c(0,1),c(0,1))
  plcf2<-plcf(c(0,1),c(1,1))
  expect_equal(corplcfs(plcf1,plcf2,NULL),NaN)

  plcf1<-plcf(c(0,1),c(0,1))
  plcf2<-plcf(c(0,1),c(1,1.5))
  expect_equal(corplcfs(plcf1,plcf2,NULL),1)
  
  plcf1<-plcf(c(0,2),c(0,2))
  plcf2<-plcf(c(0,1,2),c(0,1,0))
  expect_equal(corplcfs(plcf1,plcf2,NULL),0)
  
  plcf1<-plcf(c(0,3),c(0,3))
  plcf2<-plcf(c(0,2,3),c(0,2,0))
  num<-(8/3-5+3)+(-18+36-45/2)-(-16/3+16-15)
  denom1<-(9-6*9/4+27/4)
  denom2<-(8/3-4+2)+(4*9-90+75)-(4*8/3-40+50)
  expect_equal(corplcfs(plcf1,plcf2,NULL),num/sqrt(denom1*denom2))
  
  x<-runif(15)
  y1<-runif(15)
  y2<-(2*y1+3)
  y3<-(-3*y1+1)
  obj1<-plcf(x,y1)
  obj2<-plcf(x,y2)
  obj3<-plcf(x,y3)
  expect_equal(corplcfs(obj1,obj2,NULL),1)
  expect_equal(corplcfs(obj1,obj3,NULL),-1)

})

