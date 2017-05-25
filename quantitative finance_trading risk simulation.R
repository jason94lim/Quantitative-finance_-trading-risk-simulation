set.seed(2222)
m= 10^5 #Number of Iterations
library(MASS)

#n=5, 10^5 Iterations
ptm1= proc.time()
n1= 5  
randmatrix1= matrix(rnorm(m*n1),nrow=m) #Simulate 10^5*5 matrix of normal random variates
for (i in 1: n1) {#Simulate 10^5 iterations of X_i where i=1,2, ..., 5
  randmatrix1[,i]=.rowSums(randmatrix1,m,i)
} 
f1=function(y){ #Create indicator function for X_i>0
  if (y>0) {y=1}
  else {y=0}
} 
Indicator=apply(randmatrix1,1:2,f1) 
Indicator_Sum= apply(Indicator,1,sum)
G_5= Indicator_Sum*(1/n1)
#Frequency Table of G_5
freq=as.data.frame(table(G_5))
#Histogram
barplot(table(G_5),main="n=5, 10^5 Iterations", xlab="G_5",ylab="Frequency")
#Distribution
x=as.vector(freq[,1])
y=as.vector(freq[,2])
pmf=matrix(c(x,y*(1/m)),nrow=length(x))
colnames(pmf)=c("G_5","PMF")
pmf
proc.time()- ptm1
#PMF(n=5, 10^5 Iterations)
# G_5     PMF
# 0.0    0.33933
# 0.2    0.11455
# 0.4    0.04498
# 0.6    0.04429
# 0.8    0.11513
# 1.0    0.34172
#user  system elapsed 
#5.53    0.19    5.96 

#n=20, 10^5 Iterations
ptm2= proc.time()
n2= 20  
randmatrix2= matrix(rnorm(m*n2),nrow=m) #Simulate 10^5*20 matrix of normal random variates
for (i in 1: n2) {#Simulate 10^5 iterations of X_i where i=1,2, ..., 20
  randmatrix2[,i]=.rowSums(randmatrix2,m,i)
} 
f1=function(y){ #Create indicator function for X_i>0
  if (y>0) {y=1}
  else {y=0}
} 
Indicator=apply(randmatrix2,1:2,f1) 
Indicator_Sum= apply(Indicator,1,sum)
G_20= Indicator_Sum*(1/n2)
#Frequency Table of G_20
freq=as.data.frame(table(G_20))
#Histogram
barplot(table(G_20),main="n=20, 10^5 Iterations", xlab="G_20",ylab="Frequency")
#Distribution
x=as.vector(freq[,1])
y=as.vector(freq[,2])
pmf=matrix(c(x,y*(1/m)),nrow=length(x))
colnames(pmf)=c("G_20","PMF")
pmf
proc.time()- ptm2
#PMF(n=5, 10^5 Iterations)
#    G_20     PMF      
#    "0"    "0.34003"
#   "0.05"  "0.11365"
#   "0.1"   "0.03296"
#  "0.15"   "0.00968"
#  "0.2"    "0.00313"
#  "0.25"   "0.00078"
#  "0.3"    "0.00021"
#  "0.35"    "5e-05"  
#  "0.45"    "1e-05"  
#  "0.65"    "8e-05"  
#  "0.7"    "0.00028"
#  "0.75"   "0.00074"
#  "0.8"    "0.00278"
#  "0.85"  "0.00975"
#  "0.9"    "0.03304"
#  "0.95"  "0.11154"
#  "1"     "0.34129"
#user  system elapsed 
#19.04    0.45   20.75 

#n=500, 10^5 Iterations
ptm3= proc.time()
n3= 500  
randmatrix3= matrix(rnorm(m*n3),nrow=m) #Simulate 10^5*500 matrix of normal random variates
for (i in 1: n3) {#Simulate 10^5 iterations of X_i where i=1,2, ..., 500
  randmatrix3[,i]=.rowSums(randmatrix3,m,i)
} 
f1=function(y){ #Create indicator function for X_i>0
  if (y>0) {y=1}
  else {y=0}
}
Indicator=apply(randmatrix3,1:2,f1) 
Indicator_Sum= apply(Indicator,1,sum)
G_500= Indicator_Sum*(1/n3)
#Frequency Table of G_500
freq=as.data.frame(table(G_500))
#Histogram
barplot(table(G_500),main="n=500, 10^5 Iterations", xlab="G_500",ylab="Frequency")
#Distribution
x=as.vector(freq[,1])
y=as.vector(freq[,2])
pmf=matrix(c(x,y*(1/m)),nrow=length(x))
colnames(pmf)=c("G_500","PMF")
pmf
proc.time()- ptm3
#PMF(n=500, 10^5 Iterations)
#  G_500       PMF      
#   "0"     "0.34182"
#  "0.002"  "0.11148"
#  "0.004"  "0.03352"
#  "0.006"  "0.01041"
#  "0.008"  "0.00297"
#  "0.01"   "0.00083"
#  "0.012"  "0.00022"
#  "0.014"  "7e-05"  
#  "0.016"  "2e-05"  
#  "0.018"  "1e-05"  
#  "0.984"  "1e-05"  
#  "0.986"  "8e-05"  
#  "0.988"  "0.00028"
#  "0.99"   "0.00091"
#  "0.992"  "0.00307"
#  "0.994"  "0.0094" 
#  "0.996"  "0.03356"
#  "0.998"  "0.11062"
#    "1"    "0.34072"
#user  system elapsed 
#643.25  183.50  899.53 

#Frequency of G_n where n=5, 20, 500
as.data.frame(table(G_5))
as.data.frame(table(G_20))