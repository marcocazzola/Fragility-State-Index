#Needed libraries----
library(readxl)
library(boot)
library(matrixStats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(corrplot)
library(factoextra)
library(dendextend)
library(mclust)
library(maps)

#Data import----
data <- read_excel("fsi-2021.xlsx") %>%
  select(-3)
data$`Human Development` <- factor(data$`Human Development`,
                                   levels = c('Low', 'Medium', 'High', 'Very High'))

#Bootstrap----
###Define the function to be passed to boot()----
comp.multi.mean <- function(df, index, vars) colMeans(df[index, vars])

###Estimate a single set of indicators for a single region----
multiva_boot <- function(data, vars, hd) {
  res <- data.frame('HD' = rep(hd, length(vars)),
                    'Indicator' = colnames(data)[vars], 
                    'Estimate' = rep(NA, length(vars)), 
                    'S.E.' = rep(NA, length(vars)))
  temp <- data[data$`Human Development`==hd,]
  boot.res <- boot(temp, comp.multi.mean, R=1000, vars=vars)
  res$Estimate <- boot.res$t0
  res$S.E.<- colSds(boot.res$t)
  
  return(res)
}

###estimate a set of indicators for all the regions----
multi.table <- data.frame()
for (lvl in levels(data$`Human Development`)) {
  res <- multiva_boot(data, 3:14, lvl)
  multi.table <- rbind(multi.table, res)
}

multi.table$Inferior.Limit <- multi.table$Estimate - 1.96*multi.table$S.E.
multi.table$Superior.Limit <- multi.table$Estimate + 1.96*multi.table$S.E.

multi.table$HD <- factor(multi.table$HD, levels=c('Low', 'Medium', 'High', 'Very High'))

###Univariate Plot----
#Change the filter condition to see different variables
plot.table <- filter(multi.table, Indicator == colnames(data)[6])

ggplot(plot.table, aes(x=HD, y=Estimate, color=HD)) + geom_point(size=3) + theme_bw() +
  geom_errorbar(aes(ymin=Inferior.Limit, ymax=Superior.Limit), width=0.3) +
  scale_color_brewer(palette = "Dark2") + theme(legend.position = 'none') +
  ggtitle(unique(plot.table$Indicator)) + xlab('Human Development')

###Bivariate plot----
#Change the filter condition to see different variables
plot.table <- filter(multi.table, Indicator == colnames(data)[3] | Indicator == colnames(data)[11])

ggplot(data=NULL, aes(1:10, 1:10)) + theme_bw() + xlab(unique(plot.table$Indicator)[1]) + ylab(unique(plot.table$Indicator[2])) +
  ggtitle(paste(unique(plot.table$Indicator)[1], 'vs', unique(plot.table$Indicator)[2])) +
  annotate("rect",xmin=plot.table$Inferior.Limit[1], xmax=plot.table$Superior.Limit[1], ymin=plot.table$Inferior.Limit[2], ymax=plot.table$Superior.Limit[2], alpha=0.5, fill="#7570b4") +
  annotate("rect",xmin=plot.table$Inferior.Limit[3], xmax=plot.table$Superior.Limit[3], ymin=plot.table$Inferior.Limit[4], ymax=plot.table$Superior.Limit[4], alpha=0.5, fill="#1d9d78") +
  annotate("rect",xmin=plot.table$Inferior.Limit[5], xmax=plot.table$Superior.Limit[5], ymin=plot.table$Inferior.Limit[6], ymax=plot.table$Superior.Limit[6], alpha=0.5, fill="#d96003") +
  annotate("rect",xmin=plot.table$Inferior.Limit[7], xmax=plot.table$Superior.Limit[7], ymin=plot.table$Inferior.Limit[8], ymax=plot.table$Superior.Limit[8], alpha=0.5, fill="#e72a8a") +
  annotate("label", x=plot.table$Estimate[1], y=plot.table$Superior.Limit[2], label="High") +
  annotate("label", x=plot.table$Estimate[3], y=plot.table$Superior.Limit[4], label="Low") +
  annotate("label", x=plot.table$Estimate[5], y=plot.table$Superior.Limit[6], label="Medium") +
  annotate("label", x=plot.table$Estimate[7], y=plot.table$Superior.Limit[8], label="Very High") +
  geom_point(aes(x=plot.table$Estimate[1], y=plot.table$Estimate[2]), color="#7570b4", size=3) +
  geom_point(aes(x=plot.table$Estimate[3], y=plot.table$Estimate[4]), color="#1d9d78", size=3) +
  geom_point(aes(x=plot.table$Estimate[5], y=plot.table$Estimate[6]), color="#d96003", size=3) +
  geom_point(aes(x=plot.table$Estimate[7], y=plot.table$Estimate[8]), color="#e72a8a", size=3)

#PCA----
###Corr matrix----
cor(data[,3:14]) %>%
  corrplot(type="upper",tl.col="black",tl.srt=45, tl.cex=0.5, method='number', number.cex = 0.5)

###ScreePlot----
pca.out <- princomp(data[,3:14], cor=T)
screeplot(pca.out, type='lines', main = 'Scree plot')
abline(h=1, col='red', lwd=2)

##VarianceExplained----
var <- data.frame(Variance = pca.out$sdev^2, 
                  PVE = (pca.out$sdev^2/sum(pca.out$sdev^2))*100)
ggplot(var[1:5,], aes(x=rownames(var)[1:5], y=PVE)) +
  geom_bar(stat='identity', fill = 'steelblue') + theme_minimal() +
  geom_text(aes(label=paste0(round(PVE,2),'%')), 
            vjust = -0.3, size=3.5) + xlab('Components')

###Correlation components vs original variables----
corr.comp <- data.frame(Comp.1 = round(-pca.out$loadings[,1]*pca.out$sdev[1], 3),
                        Comp.2 = round(-pca.out$loadings[,2]*pca.out$sdev[2], 3)) %>%
  mutate(Communality = round(Comp.1^2 + Comp.2^2,3)) %>%
  as.matrix()
corrplot(t(corr.comp), method='number', is.corr=F, tl.col="black", tl.srt=45, number.cex=0.7)

###Scores----
fviz_cluster(object = list(data= data[,3:14], cluster = data$`Human Development`),
             geom = 'point', shape=19, ggtheme = theme_bw(), show.clust.cent=FALSE, pointsize=2) +
  geom_hline(aes(yintercept=0), linetype='dashed') + ggtitle('PCA Representation') + guides(fill=guide_legend("Human Development"), color=guide_legend("Human Development")) +
  geom_vline(aes(xintercept=0), linetype='dashed') + theme(legend.position = 'top')

#MDS----

###Euclidean----
D <- dist(data[,3:14])
coord <- as.data.frame(cbind(cmdscale(D))) %>%
  mutate(HD = data$`Human Development`)

ggplot(coord, aes(V1, V2)) + geom_point(aes(color=HD), size=3) +
  theme_bw() + 
  geom_hline(aes(yintercept=0), linetype='dashed') +
  geom_vline(aes(xintercept=0), linetype='dashed') +
  guides(color=guide_legend("Human Development")) + 
  theme(legend.position = 'top') + ggtitle('MDS with Euclidean distance')
  
#Same as PCA!

###Manhattan----
D <- dist(data[,3:14], method='manhattan')
coord <- as.data.frame(cmdscale(D)) %>%
  mutate(HD = data$`Human Development`)

ggplot(coord, aes(V1, -V2)) + geom_point(aes(color=HD), size=3) +
  theme_bw() + 
  geom_hline(aes(yintercept=0), linetype='dashed') +
  geom_vline(aes(xintercept=0), linetype='dashed') +
  guides(color=guide_legend("Human Development")) + 
  theme(legend.position = 'top') + ggtitle('MDS with Manhattan distance')

###Maximum----
D <- dist(data[,3:14], method='maximum')
coord <- as.data.frame(cmdscale(D)) %>%
  mutate(HD = data$`Human Development`)

ggplot(coord, aes(V1, V2)) + geom_point(aes(color=HD), size=3) +
  theme_bw() + 
  geom_hline(aes(yintercept=0), linetype='dashed') +
  geom_vline(aes(xintercept=0), linetype='dashed') +
  guides(color=guide_legend("Human Development")) + 
  theme(legend.position = 'top') + ggtitle('MDS with Maximum distance')

#K-means----
tot.withinss <- NULL
n <- dim(data)[1]
for (k in 1:10) {
  set.seed(123)
  km.out <- kmeans(data[,-c(1:2)], k, nstart = 50)
  tot.withinss[k] <- km.out$tot.withinss
}
plot(1:10, tot.withinss, type='b', xlab='Nr of clusters')

###K-Means with 4 clusters----

set.seed(123)
km.out <- kmeans(data[,-c(1:2)],4,nstart=50)

###MAP----
cluster.data <- data.frame("region"=data$Country, 
                           "Cluster"=as.factor(km.out$cluster))
old.names <- c(4, 25, 27, 141, 148, 151)
new.names <- c("Democratic Republic of the Congo","Republic of Congo","Ivory Coast",
               "USA","UK","Czech Republic")

cluster.data$region<-replace(cluster.data$region,old.names,new.names)

world.map <- map_data("world")

cluster.data.map <- full_join(cluster.data, world.map, by = "region")

ggplot(cluster.data.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Cluster, color=Cluster), color="black")+ 
  scale_fill_discrete(na.value="darkgray") +
  theme_minimal() + theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank())

###Clusters in PC space----

fviz_cluster(km.out, data=data[,-c(1:2)], 
             geom = c("point"), show.clust.cent=TRUE) +
  theme_minimal() + geom_vline(xintercept = 0, lty="dashed") +
  geom_hline(yintercept = 0, lty="dashed")

###Tab----
df <- melt(table("Human.Development"=data$`Human Development`, "Cluster"=km.out$cluster))
df$Human.Development <- factor(df$Human.Development, levels = c('Low', 'Medium', 'High', 'Very High'))
ggplot(df, aes(Cluster, Human.Development, fill=value)) + geom_tile(color='black') +
  geom_text(aes(label = value), color = "black", size = 4) + scale_fill_gradient(low="white",high="brown3") + 
  theme_minimal()

#Model-based clustering----
mc <- Mclust(data[,3:14])

###Tab----
df <- melt(table("Human.Development"=data$`Human Development`, "Cluster"=mc$classification))
df$Human.Development <- factor(df$Human.Development, levels = c('Low', 'Medium', 'High', 'Very High'))
ggplot(df, aes(Cluster, Human.Development, fill=value)) + geom_tile(color='black') +
  geom_text(aes(label = value), color = "black", size = 4) + scale_fill_gradient(low="white",high="brown3") + 
  theme_minimal()

#Not good, as 3 Very High HD countries are put together with
#Low HD countries. 

###Let's try with 3----
mc <- Mclust(data[,3:14],G=3)

###Tab----
df <- melt(table("Human.Development"=data$`Human Development`, "Cluster"=mc$classification))
df$Human.Development <- factor(df$Human.Development, levels = c('Low', 'Medium', 'High', 'Very High'))
ggplot(df, aes(Cluster, Human.Development, fill=value)) + geom_tile(color='black') +
  geom_text(aes(label = value), color = "black", size = 4) + scale_fill_gradient(low="white",high="brown3") + 
  theme_minimal()

###Clusters in PC space----
fviz_mclust(mc, 'classification', geom='point', show.clust.cent=F)

###MAP----
cluster.data <- data.frame("region"=data$Country, 
                           "Cluster"=as.factor(mc$classification))
old.names <- c(4, 25, 27, 141, 148, 151)
new.names <- c("Democratic Republic of the Congo","Republic of Congo","Ivory Coast",
               "USA","UK","Czech Republic")

cluster.data$region<-replace(cluster.data$region,old.names,new.names)

world.map <- map_data("world")

cluster.data.map <- full_join(cluster.data, world.map, by = "region")

ggplot(cluster.data.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Cluster, color=Cluster), color="black")+ 
  scale_fill_discrete(na.value="darkgray") +
  theme_minimal() + theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank())

###K-means vs Model-based clust----
df <- melt(table("MBCl"=factor(mc$classification), "K.means"=factor(km.out$cluster)))
ggplot(df, aes(MBCl, K.means, fill=value)) + geom_tile(color='black') +
  geom_text(aes(label = value), color = "black", size = 4) + scale_fill_gradient(low="white",high="brown3") + 
  theme_minimal() + xlab('Model-based Clust')