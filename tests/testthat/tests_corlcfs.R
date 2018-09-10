context("corlcfs")

test_that("test the error handling",{
lcf1<-lcf(c(0.5,1.5,2.5,3.5,4.5),c(3,4,3,4))
lcf2<-lcf(c(0,1,2,3,4,5),c(1,2,1,2,1))
expect_error(corlcfs(lcf1,lcf2,"test"),
             "Error in corlcfs: bds argument must be numeric or NULL")
expect_error(corlcfs(lcf1,lcf2,1),
             "Error in corlcfs: bds argument must be a length-2 numeric vector or NULL")
expect_error(corlcfs(lcf1,lcf2,c(1,5)),
             "Error in corlcfs: bds must not include values for which one or both of obj1 or obj2 is not defined")

lcf1<-lcf(c(0.5,1.5,2.5,3.5,4.5),c(3,4,3,4))
lcf2<-lcf(c(0,1,2,3,4,5)+10,c(1,2,1,2,1))
expect_error(corlcfs(lcf1,lcf2,NULL),
             "Error in corlcfs: obj1 and obj2 not both defined for any range of values")
})

test_that("test the quantitative accuracy of the output",{
obj1<-lcf(c(0.5,1.5,2.5,3.5,4.5),c(3,4,3,4))
obj2<-lcf(c(0,1,2,3,4,5),c(1,2,1,2,1))
bds<-NULL
expect_equal(corlcfs(obj1,obj2,NULL),0)

obj1<-lcf(c(0,1,2,3,4),c(1,2,0,1.5))
obj2<-lcf(c(0.5,1.5,3.5),c(0,1))
expect_equal(corlcfs(obj1,obj2),(-30/72)/sqrt((606/288)*(12/18)))
})