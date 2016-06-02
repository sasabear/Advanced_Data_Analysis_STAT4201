#hw2 

library(MASS)
crabs
blue<-crabs[crabs$sp=="B",]
orange<-crabs[crabs$sp=="O",]

b_cl<-blue$CL
o_cl<-orange$CL

hist(b_cl,main="histogram of blue crab CL")
hist(o_cl,main="histogram of orange  CL")
boxplot(b_cl,main="boxplot of blue crab CL")
boxplot(o_cl,main="boxplot of orange crab CL")
qqnorm(b_cl,main="Q-Q plot of blue crab CL")
qqline(b_cl)
qqnorm(o_cl,main="Q-Q plot of orange crab CL")
qqline(o_cl)

# two sample t test
t.test(o_cl,b_cl, var.equal=T)

# F test for two varaince 

var.test(o_cl,b_cl)

# welch's modified two sample t test

t.test(o_cl,b_cl)

# wilcoxon rank-sum test
wilcox.test(o_cl,b_cl)


# bootstrap 
b_cl_new<-b_cl-mean(b_cl)
o_cl_new<-o_cl-mean(o_cl)
z_obs<-(mean(b_cl)-mean(o_cl))/sqrt((var(b_cl)/100)+(var(o_cl)/100))


z<-rep(0,1000)
for(i in 1:1000)
{
        x <- sample(b_cl_new,replace=T)
        y <- sample(o_cl_new,replace=T)
        avg<- mean(x)-mean(y)
        var_x<-var(x)
        var_y<-var(y)
        z[i]<-avg/(sqrt((var_x/100)+(var_y/100)))
}

pa<-(sum(abs(z)>=abs(z_obs)))/1000

#p3

x<-c(48,68)
n<-c(197,197)
prop.test(x,n)

data<-matrix(c(149,48,129,68),2,2)
chisq.test(data)
fisher.test(data)



#p4

data_1<-array(c(38,26,52,61,95,150,189,255),c(2,2,2))
mantelhaen.test(data_1)

data_2<-matrix(c(21,3,14,10),2,2)
chisq.test(data_2)
fisher.test(data_2)

data_3<-matrix(c(65,147,94,153),2,2)
fisher.test(data_3)

data_4<-matrix(c(30,42,56,102),2,2)
fisher.test(data_4)
