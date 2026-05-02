library(mvtnorm) # for simulating convex data

set.seed(865)

# convex data with two non-overlapping clusters and consistent cluster size
# generate two clusters that are convex, non-overlapping
sig1_1 <- diag(3)
sig1_2 <- diag(6.25,3)
x1_1 <- rmvnorm(100,c(0,0,0),sig1_1)
x1_2 <- rmvnorm(100,c(8,8,8),sig1_2)

X <- rbind(x1_1,x1_2)
           
trueX <- rep(1:2,each=100) # generate labels for data

#---------------------------
# convex data with two overlapping clusters and consistent cluster size
# generate two 3d clusters that are convex, overlapping
sig2_1 <- diag(6,3)
sig2_2 <- diag(0.75,3)
x2_1 <- rmvnorm(100,c(-9,9,9),sig2_1)
x2_2 <- rmvnorm(100,c(-10,11,11),sig2_2)

X2 <- rbind(x2_1,x2_2)

trueX2 <- rep(3:4,each=100)

#---------------------------
# non-convex data in the shape of a triple helix with three non-overlapping 
# clusters and consistent cluster size

t <- seq(-1,8*pi,length.out=200) * rnorm(200,mean=1,sd=0.04)

# cluster 1
x3_1 <- cos(t)
x3_2 <- sin(t)
x3_3 <- t * rnorm(200,mean=1,sd=0.01)

# cluster 2
x3_4 <- cos(t+ 2*pi/3)
x3_5 <- sin(t+ 2*pi/3)
x3_6 <- t * rnorm(200,mean=1,sd=0.01)

# cluster 3
x3_7 <- cos(t+ 4*pi/3)
x3_8 <- sin(t+ 4*pi/3)
x3_9 <- t * rnorm(200,mean=1,sd=0.01)

X3_1 <- cbind(x3_1,x3_2,x3_3)
X3_2 <- cbind(x3_4,x3_5,x3_6)
X3_3 <- cbind(x3_7,x3_8,x3_9)

X3 <- rbind(X3_1,X3_2,X3_3)

trueX3 <- rep(5:7,each=200)


#---------------------------
# non-convex data of a saddle and spring forming two overlapping clusters
# with consistent cluster size, n=500

# simulate data in shape of saddle, cluster 1
x4_1 <- runif(500,-3,3)
x4_2 <- runif(500,-3,3)
x4_3 <- x4_1^2 - x4_2^2

# simulate data in helix shape, cluster 2
t <- seq(-10,2*pi,length.out=500) * rnorm(500,mean=1,sd=0.04)

x4_4 <- cos(t)
x4_5 <- sin(t)
x4_6 <- t * rnorm(500,mean=1,sd=0.01)

X4_1 <- cbind(x4_1,x4_2,x4_3)
X4_2 <- cbind(x4_4,x4_5,x4_6)

X4 <- rbind(X4_1,X4_2)

trueX4 <- rep(c('purple','yellow'),each=500)


#---------------------------
# convex data with two non-overlapping clusters and inconsistent cluster size

# generate two clusters that are convex, non-overlapping
x1i_1 <- rmvnorm(150,c(0,0,0),sig1_1) # cluster 1, n=150
x1i_2 <- rmvnorm(60,c(8,8,8),sig1_2) # cluster 2, n=60

Xi <- rbind(x1i_1,x1i_2)
           
trueXi <- c(rep(1,150),rep(2,60)) # generate labels for data


#---------------------------
# convex data with two non-overlapping clusters and inconsistent cluster size

# generate two clusters that are convex, non-overlapping (sizes flipped)
x1i2_1 <- rmvnorm(60,c(0,0,0),sig1_1) # cluster 1, n=60
x1i2_2 <- rmvnorm(150,c(8,8,8),sig1_2) # cluster 2, n=150

Xi2 <- rbind(x1i2_1,x1i2_2)
           
trueXi2 <- c(rep(1,60),rep(2,150)) # generate labels for data


#---------------------------
# convex data with two overlapping clusters and inconsistent cluster size

# generate two 3d clusters that are convex, overlapping
x2i_1 <- rmvnorm(150,c(-8,8,8),sig2_1) # cluster 1, n=150
x2i_2 <- rmvnorm(60,c(-10,11,11),sig2_2) # cluster 2, n=60

X2i <- rbind(x2i_1,x2i_2)

trueX2i <- c(rep(3,150),rep(4,60))


#---------------------------
# convex data with two overlapping clusters and inconsistent cluster size

# generate two 3d clusters that are convex, overlapping (sizes flipped)
x2i2_1 <- rmvnorm(60,c(-8,8,8),sig2_1) # cluster 1, n=60
x2i2_2 <- rmvnorm(150,c(-10,11,11),sig2_2) # cluster 2, n=150

X2i2 <- rbind(x2i2_1,x2i2_2)

trueX2i2 <- c(rep(3,60),rep(4,150))


#---------------------------
# non-convex data in the shape of a triple helix with three non-overlapping 
# clusters and inconsistent cluster size

# cluster 1, n=500
t1i <- seq(-1,8*pi,length.out=500) * rnorm(500,mean=1,sd=0.04)

x3i_1 <- cos(t1i)
x3i_2 <- sin(t1i)
x3i_3 <- t1i * rnorm(500,mean=1,sd=0.01)

# cluster 2, n=200
t2i <- seq(-1,8*pi,length.out=200) * rnorm(200,mean=1,sd=0.04)

x3i_4 <- cos(t2i+ 2*pi/3)
x3i_5 <- sin(t2i+ 2*pi/3)
x3i_6 <- t2i * rnorm(200,mean=1,sd=0.01)

# cluster 3, n=100
t3i <- seq(-1,8*pi,length.out=100) * rnorm(100,mean=1,sd=0.04)

x3i_7 <- cos(t3i+ 4*pi/3)
x3i_8 <- sin(t3i+ 4*pi/3)
x3i_9 <- t3i * rnorm(100,mean=1,sd=0.01)

X3i_1 <- cbind(x3i_1,x3i_2,x3i_3)
X3i_2 <- cbind(x3i_4,x3i_5,x3i_6)
X3i_3 <- cbind(x3i_7,x3i_8,x3i_9)

X3i <- rbind(X3i_1,X3i_2,X3i_3)

trueX3i <- c(rep(5,500),rep(6,200),rep(7,100))


#---------------------------
# non-convex data of a saddle and spring forming overlapping clusters with
# inconsistent cluster size

# simulate data in shape of saddle, cluster 1 n=400
x4i_1 <- runif(400,-4,4)
x4i_2 <- runif(400,-4,4)
x4i_3 <- x4i_1^2 - x4i_2^2

# simulate data in helix shape, cluster 2 n=150
ti <- seq(-10,2*pi,length.out=150) * rnorm(150,mean=1,sd=0.04)

x4i_4 <- cos(ti)
x4i_5 <- sin(ti)
x4i_6 <- ti * rnorm(150,mean=1,sd=0.01)

X4i_1 <- cbind(x4i_1,x4i_2,x4i_3)
X4i_2 <- cbind(x4i_4,x4i_5,x4i_6)

X4i <- rbind(X4i_1,X4i_2)

trueX4i <- c(rep('purple',400),rep('yellow',150))


#---------------------------
# non-convex data of a saddle and spring forming two overlapping clusters
# with inconsistent cluster size (sizes flipped)

# simulate data in shape of saddle, cluster 1 n=150
x4i2_1 <- runif(150,-4,4)
x4i2_2 <- runif(150,-4,4)
x4i2_3 <- x4i2_1^2 - x4i2_2^2

# simulate data in helix shape, cluster 2 n=400
ti2 <- seq(-10,2*pi,length.out=400) * rnorm(400,mean=1,sd=0.04)

x4i2_4 <- cos(ti2)
x4i2_5 <- sin(ti2)
x4i2_6 <- ti2 * rnorm(400,mean=1,sd=0.01)

X4i2_1 <- cbind(x4i2_1,x4i2_2,x4i2_3)
X4i2_2 <- cbind(x4i2_4,x4i2_5,x4i2_6)

X4i2 <- rbind(X4i2_1, X4i2_2)

trueX4i2 <- c(rep('purple',150),rep('yellow',400))

