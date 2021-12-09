
setwd("~/Desktop/  /FA-21/Huge Data/Course Project")
library(ggplot2)
library(Cairo)
library(factoextra)
# library(useful)
# library(tidyr)
library(plotly)
library(dplyr)
library(dbscan)

names = c("Index","LastName","FirstName","DatasetID","Dataset","SpeciesName","AccSpeciesID","AccSpeciesName","ObservationID","ObsDataID","TraitID","TraitName","DataID","DataName","OriglName","OrigValueStr","OrigUnitStr","ValueKindName","OrigUncertaintyStr","UncertaintyName","Replicates","StdValue","UnitName","RelUncertaintyPercent","OrigObsDataID","ErrorRisk","Reference","Comment","V28")
# df = read.csv("trait_env_values.csv", col.names=names, fileEncoding='iso-8859-1', nrows=1681253)
# iconv(df, from="iso-8859-1", to="ASCII")
# write.csv(df, file="trait_env_values_ascii.csv")
df = read.csv("trait_env_values_ascii.csv", col.names=names, nrows=1681253)

# To get feature-wise comparison
cols = c('AccSpeciesID', 'TraitID', 'DataID', 'OrigValueStr', 'OrigUnitStr')
s = sample(c(1:length(df[,1])), size=20000)
u.vals = unique(df$OrigUnitStr)
# u.inds = which(u.vals!="")
# u.vals = u.vals[u.inds)]
u.vals = u.vals[which(u.vals!="")]



# Cairo example
Cairo::Cairo(
  30, #length
  30, #width
  file = paste("Scatter", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
pairs(df[s,cols]) #, xlim=c(-0, 1000000), ylim=c(-0, 1000000))
dev.off()


# 
# cols = c('TraitID', 'OrigValueStr', 'OrigUnitStr')
# print(df[sample(c(1:length(df[,1])), size=15),cols])
# s = sample(c(1:length(df[,1])), size=20000)
# temp = df[s,cols]
# for (x in u.vals) {
#   inds = which(temp$OrigUnitStr==x)
#   if (length(inds) > 30) {
#     pairs(temp[inds,], xlim=c(-0, 400000), ylim=c(0, 400000))
#   }
# }


# From here for plot


Cairo::Cairo(
  30, #length
  30, #width
  file = paste("Scatter", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)


cols = c('AccSpeciesID','TraitID', 'OrigValueStr', 'OrigUnitStr')
temp = df[sample(c(1:length(df[,1])), size=20000),cols]
out = c()
# test = 'cm'
for (x in u.vals) {
  if (length(which(temp$OrigUnitStr==x)) > 1) {
  # if (length(which(temp$OrigUnitStr==test)) > 500) {
    # print(x)
    out = c(out, which(temp$OrigUnitStr == x))
    # out = c(out, which(temp$OrigUnitStr == test))
  }
}
temp = temp[out,]
temp$OrigValueStr = as.numeric(temp$OrigValueStr)
# print(unique(temp$OrigUnitStr))
# p <- ggplot(data = diamonds, aes(x = price, y = carat, z=AccSpeciesID, colour = color)) + geom_point()
# ggplot(data=temp, aes(x=AccSpeciesID, y=OrigValueStr, z=OrigUnitStr, colour=TraitID)) + geom_point()
ggplot(data=temp, aes(x=AccSpeciesID, y=OrigValueStr, colour=TraitID)) + geom_point()
dev.off()

# cols = c('AccSpeciesID', 'TraitID', 'TraitName')
# s = sample(c(1:length(df[,1])), size=20000)
# pairs(df[s,cols], xlim=c(-10000, 10000), ylim=c(-10000, 10000))


# 
# Clustering Methods
# 
cols = c('TraitID', 'OrigValueStr', 'AccSpeciesID', 'OrigUnitStr')
temp = df[sample(c(1:length(df[,1])), size=200000),cols]
out = c()
# test = 'cm'
for (str.unit in u.vals) {
  out = c(out, which(temp$OrigUnitStr == str.unit))
}
temp = temp[out,]
# temp <- replace_na(temp, list(-1,-1, -1, 'cm'))
temp = remove_missing(temp, na.rm=TRUE)
norm_func = function(vals) {
  vals = (vals - min(vals)) / (max(vals) - min(vals))
}
# cols = c('TraitID', 'OrigValueStr', 'OrigUnitStr')
# temp = temp[,cols]
temp$OrigValueStr = as.numeric(as.character(temp$OrigValueStr))
temp = remove_missing(temp, na.rm=TRUE)
temp[,1:3] = as.data.frame(lapply(temp[,1:3], norm_func))
units = temp$OrigUnitStr
species = temp$AccSpeciesID
cols = c('TraitID', 'OrigValueStr')
temp = temp[,cols]

# Per-unit graph
# Cairo::Cairo(
#   30, #length
#   30, #width
#   file = paste("Scatter", ".png", sep = ""),
#   type = "png", #tiff
#   bg = "white", #white or transparent depending on your requirement
#   dpi = 300,
#   units = "cm" #you can change to pixels etc 
# )

count = 0
test = '%'
for (unit in u.vals) {
  count = count + 1
  K=4
  # if (count < 3) {
  #   K=2
  # }
  if (unit == test) {
    tmp = temp[which(units==unit),]
    if (length(tmp$OrigValueStr) > 10) {
      print(unit)
    }
    t.species = species[which(units==test)]
    cluster <- kmeans(tmp, K)
    cluster$cluster = as.factor(cluster$cluster)
    p = plot_ly(x=~tmp$OrigValueStr, y=~tmp$TraitID, z=~t.species, color=~cluster$cluster) %>%
      add_markers(size=1.5) %>%
      layout(title="Plant hydration vs. Plant trait", scene=list(xaxis = list(title = 'Plant Hydration'), yaxis = list(title = 'Trait ID'), zaxis=list(title='Species') ))
    # cluster$totss
    print(p)
  }
}
# dev.off()

# Run
units=as.numeric(factor(units, levels=u.vals))
cluster = kmeans(temp, 6)
p = plot_ly(x=~temp$OrigValueStr, y=~temp$TraitID, 
             z=~units, color=~cluster$cluster) %>%
  add_markers(size=1.5) %>%
  layout(scene = list(xaxis = list(title = 'Measurement Value'), yaxis = list(title = 'Trait ID'), zaxis = list(title = 'Measurement ID')))
# cluster$totss
# cluster$totss
print(p)

# cluster <- kmeans(c(temp$TraidID, temp$OrigValueStr, temp$AccSpeciesID), 10)
# plot the clusters
# fviz_cluster(cluster, data=temp, geom=c("point"), ellipse.type = "euclid")
# cluster = remove_missing(cluster, na.rm=TRUE)
# plot.kmeans(x=cluster, data=temp)




tmp = temp[which(units=='%'),]
t.species = species[which(units=='%')]
# eps_plot = kNNdistplot(tmp, k=4000)
t = dbscan(tmp, eps=0.00005, minPts=3)
# fviz_cluster(d, customer_prep, geom = "point")
t$cluster = as.factor(t$cluster)
p = plot_ly(x=~tmp$OrigValueStr, y=~tmp$TraitID, z=~t.species, color=~t$cluster) %>%
  add_markers(size=1.5) %>%
  layout(title="Plant hydration vs. Plant trait", scene=list(xaxis = list(title = 'Plant Hydration'), yaxis = list(title = 'Trait ID'), zaxis=list(title='Species') ))
# cluster$totss
print(p)




