# Code to graph and manipulate in 3-dimensions the bivariate normal distribution
# Demonstration of rgl package in R

library(rgl)

# Parameters for BVN, change as you see fit. 

rho=0.5
mux=0
muy=0
sigmax=1
sigmay=1

# Graphing Parameters
#	deviations is the number of std. deviations included in the plot
#	partitions^2 is the total number of dots in the plot, more=blacker
#     You want to leave a lattice in areas of interest, so choose accordingly. 

partitions=500
deviations=4

v1<-rep(seq((mux-deviations*sigmax), (mux+deviations*sigmax), by=((2*deviations*sigmax)/partitions)), times=partitions)
v2<-seq((muy-deviations*sigmay), (muy+deviations*sigmay), by=((2*deviations*sigmay)/partitions))
v3<-rep(v2, each=partitions)

# The rgl package must be installed and loaded for the following code to work!!!
# The resulting plot can be rotated by clicking and draging, the mouse wheel zooms in and out. 
# Full screen is best!
plot3d(v1[1:length(v1)], v3[1:length(v3)], (1/(2*pi*sigmax*sigmay*(1-rho^2)^(.5))*exp((-1)*(1/(2*(1-rho^2)))*((((v1[1:length(v1)]-mux)/sigmax)^2)+(((v3[1:length(v3)]-muy)/sigmay)^2)-(2*rho*(((v1[1:length(v1)]-mux)*(v3[1:length(v3)]-muy))/(sigmax*sigmay)))))), 
       xlab="X", ylab="Y", zlab="f(x,y)", main="Bivariate Normal Distribution")
