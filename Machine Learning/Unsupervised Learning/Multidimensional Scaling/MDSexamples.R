###################################################
#
#      Examples of MDS
#
##################################################

library(MASS) # for isoMDS
swiss

# Calculate a distance matrix 

d.swiss = dist(swiss)
dim(d.swiss)

# dist does not return a matrix
str(d.swiss)

# note that 47*46/2 = 1081
# we only need this much data to store all the distance pairs

d.euclid = as.matrix(dist(swiss))
d.euclid[1:5,1:5]

d.manhatten = as.matrix(dist(swiss,method = "manhattan"))
d.manhatten[1:5,1:5]

d.canberra = as.matrix(dist(swiss,method = "canberra"))
d.canberra[1:5,1:5]

# Use isoMDS

swiss.mds = isoMDS(dist(swiss)) # default is Euclidean
str(swiss.mds)
head(swiss.mds$points)

#  Two plot Styles
dev.new()
plot(swiss.mds$points, type = "n")
text(swiss.mds$points, label = as.character(1:nrow(swiss)))

dev.new()
plot(swiss.mds$points, type = "n")
text(swiss.mds$points, label = rownames(swiss), col = "red")


swiss.mds = isoMDS(dist(swiss, method = "canberra")) # override default
str(swiss.mds)
head(swiss.mds$points)

#  Two plot Styles
dev.new()
plot(swiss.mds$points, type = "n")
text(swiss.mds$points, label = as.character(1:nrow(swiss)))

dev.new()
plot(swiss.mds$points, type = "n")
text(swiss.mds$points, label = rownames(swiss), col = "red")


#### Use classical MDS

cmds.swiss = cmdscale(dist(swiss))
str(cmds.swiss)
head(cmds.swiss)
rownames(cmds.swiss)

dev.new()
plot(cmds.swiss, type = "n")
text(cmds.swiss, label = as.character(1:nrow(swiss)))

dev.new()
plot(cmds.swiss, type = "n")
text(cmds.swiss, label = rownames(swiss), col = "red")




lfs = LifeCycleSavings
head(lfs)

cmds.lfs = cmdscale(dist(lfs))

dev.new()
plot(cmds.lfs, type = "n")
text(cmds.lfs, label = rownames(lfs), col = "red")


isomds.lfs = isoMDS(dist(lfs))

dev.new()
plot(isomds.lfs$points, type = "n")
text(isomds.lfs$points, label = rownames(lfs), col = "red")

####  plantTraits requires that package cluster be loaded.

library(cluster)
pt = plantTraits
head(pt)

cmds.pt = cmdscale(dist(pt))


dev.new()
plot(cmds.pt, type = "n")
text(cmds.pt, label = rownames(pt), col = "red")









