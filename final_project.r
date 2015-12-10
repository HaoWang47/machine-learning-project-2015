
######################################################################
##########            Data  Processing                  ##############
######################################################################
#import the data
require("igraph")
setwd("/Users/Tony/Desktop/2015 FALL/Machine Learning/final propject")
node<-read.csv("medicine_node.csv")
edge<-read.csv("medicine_edge.csv")
bobc <- lapply(edge[,1:8], as.character, classes="factor", how="replace")

source<-c()
target<-c()  

for(i in 1:dim(edge)[1])
{ perm<-c()
  for (j in 1:8) perm<-c(perm,bobc[[j]][i])
  source<-c(source,combn(perm,2)[1,])
  target<-c(target,combn(perm,2)[2,])
}

df<-cbind(source,target)
df<-df[-union(which(source==""),which(target=="")),]
colnames(df)<-c("Source","Target")
write.csv(df,"edges_output.csv")
#export for gephi

net<-graph.edgelist(df,directed=F)
adj<-get.adjacency(net)

#match the colnames and the input node classes/names
class<-rep(NA,length(colnames(adj)))
for( i in 1:length(colnames(adj)))
  for(j in 1:dim(node)[1])
    if(colnames(adj)[i]==node$Medicines[j])
    {    class[i]=as.character(node$Class[j])    
    }  



######################################################################
##########            Implementing Clustering           ##############
######################################################################

#implement cluster method from kknn
install.packages("kknn")
require(kknn)

#inverse the weight
for (i in 1: rnows(adj))
  for (j in 1: nrows(adj)) 
    if(adj!=0) adj=adj^-1
    
#random walk
set.seed(1)
cl <- specClust(as.matrix(adj), nn=2,method="random-walk") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=3,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=4,method="random-walk") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=5,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=6,method="random-walk") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=7,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=8,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=9,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=10,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=11,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=12,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1)
cl <- specClust(as.matrix(adj), nn=13,method="random-walk")
plot(1:length(cl$eigenvalue),cl$eigenvalue)

#symmetric
set.seed(1234)
cl <- specClust(as.matrix(adj), nn=2,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj),center=7, nn=3,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=4,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=5,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=6,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=7,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=8,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=9,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=6,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=7,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=8,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=7,nn=9,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)

cl <- specClust(as.matrix(adj),center=14, nn=3,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=14,nn=4,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=14,nn=5,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=14,nn=6,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=14,nn=7,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=14,nn=8,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=14,nn=9,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)
set.seed(1234)
cl <- specClust(as.matrix(adj), center=14,nn=10,method="symmetric")
plot(1:length(cl$eigenvalue),cl$eigenvalue)

#unnormalized
cl <- specClust(as.matrix(adj),nn=2,method="none")
cl <- specClust(as.matrix(adj),nn=3,method="none")
cl <- specClust(as.matrix(adj),nn=4,method="none")
cl <- specClust(as.matrix(adj),nn=5,method="none")
cl <- specClust(as.matrix(adj),nn=6,method="none")
cl <- specClust(as.matrix(adj),nn=7,method="none")
cl <- specClust(as.matrix(adj),nn=8,method="none")
cl <- specClust(as.matrix(adj),nn=9,method="none")
cl <- specClust(as.matrix(adj),nn=10,method="none")
cl <- specClust(as.matrix(adj),nn=11,method="none")
cl <- specClust(as.matrix(adj),nn=12,method="none")
cl <- specClust(as.matrix(adj),nn=13,method="none")
cl <- specClust(as.matrix(adj),nn=14,method="none")
cl <- specClust(as.matrix(adj),nn=15,method="none")
cl <- specClust(as.matrix(adj),nn=16,method="none")

table(cl$cluster)
table(class,cl$cluster)

#match the result colnames and the input node names
df2<-data.frame(Nodes=colnames(adj),cluster=cl$cluster)
#export clustering result for gephi
write.csv(df2,"node_result.csv")

#fpc method 
library(cluster)
library(fpc)
clus <- kmeans(as.matrix(adj), centers=3)
plotcluster(as.matrix(adj), clus$cluster)

#Delete top two nodes that are connected to everywhere 
hist(rowSums(as.matrix(adj)))
sort(rowSums(as.matrix(adj)))
#dihuang 131
#danggui 137

dadj<-as.matrix(adj)[-which(rownames(as.matrix(adj))=="danggui"),
                     -which(rownames(as.matrix(adj))=="danggui")]
dadj<-dadj[-which(rownames(as.matrix(dadj))=="dihuang"),
           -which(rownames(as.matrix(dadj))=="dihuang")]
org<-adj
adj<-dadj

#more aggressive deletion
#fangfeng 81
#zhizi 81
#jinyinhua 87
#lianqiao 88
#baishu 88
#chuanxiong 91
#huanglian 100      
#huangqin 116

#Re-load the deleted data set -"Danggui" and -"Dihuang"
#el<-read.csv("edges_output.csv",head=T)
el<-read.csv("edges_output-12.csv",head=T)
net<-graph.edgelist(as.matrix(el[,1:2]),directed=F)
adj<-get.adjacency(net)
net<-graph.edgelist(as.matrix(el[,1:2]),directed=F)
adj<-get.adjacency(net)

#match the colnames and the input node classes/names
class<-rep(NA,length(colnames(adj)))
for( i in 1:length(colnames(adj)))
  for(j in 1:dim(node)[1])
    if(colnames(adj)[i]==node$Medicines[j])
    {    class[i]=as.character(node$Class[j])    
    }  

#start to cluster again
######################################################################################
######################           Test Performance          ###########################
######################################################################################
#test performance:
test<-read.csv("test.csv",head=T)
t.result<-c()
for(i in 1:dim(test)[1])
{ r1=result$cluster[which(as.character(result$medicine)==as.character(test[i,1]))]
  r2=result$cluster[which(as.character(result$medicine)==as.character(test[i,2]))]
  #print(paste(r1," ",i))
  #print(paste(r2," ",i))
  t.result<-c(t.result,r1==r2)
}

#yield a confusion matrix and document it
table(t.result,test[,3])/rowSums(table(t.result,test[,3]))

#match the colnames and the input node classes/names
class<-rep(NA,length(colnames(adj)))
for( i in 1:length(colnames(adj)))
  for(j in 1:dim(node)[1])
    if(colnames(adj)[i]==node$Medicines[j])
    {    class[i]=as.character(node$Class[j])    
    }  


#start to cluster
#cl <- specClust(as.matrix(adj),centers=4, nn=4, method="symmetric")
#cl <- specClust(as.matrix(adj),nn=5,method="")


lm<-laplacian_matrix(net)

set.seed(1234)
cl <- specClust(as.matrix(adj), nn=3,method="symmetric") 
plot(1:length(cl$eigenvalue),cl$eigenvalue)
#optimal cluster is 7

cl <- specClust(as.matrix(adj), center=14,nn=4,method="symmetric") 
table(cl$cluster)

table(class,cl$cluster)
sort(colSums(as.matrix(adj)))


###############################################################
#load the deleted data set -"Danggui" and -"Dihuang" and -"huangqin"
el<-read.csv("edges_output-123.csv",head=T)
net<-graph.edgelist(as.matrix(el[,1:2]),directed=F)
adj<-get.adjacency(net)


#match the colnames and the input node classes/names
class<-rep(NA,length(colnames(adj)))
for( i in 1:length(colnames(adj)))
  for(j in 1:dim(node)[1])
    if(colnames(adj)[i]==node$Medicines[j])
    {    class[i]=as.character(node$Class[j])    
    }  

#Other methods:
#betweenness centrality
df<-read.csv("edges_output-12.csv")
require("igraph")
net<-graph.edgelist(as.matrix(df[,1:2]),directed=F)
cl1<-cluster_edge_betweenness(net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{  r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
   r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
   new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))

#Information map community detection

cl1<-cluster_infomap(net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{	r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
  r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
  new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))

#propagation labels
cl1<-cluster_label_prop (net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{	r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
  r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
  new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))


#leading eigenvector
cl1<-cluster_leading_eigen (net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{	r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
  r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
  new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))


#leading eigenvector
cl1<-cluster_leading_eigen (net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{	r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
  r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
  new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))



#Multilevel-optimiazaiotn
cl1<-cluster_spinglass (net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{	r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
  r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
  new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))

#short random walk

cl1<-cluster_walktrap(net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{	r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
  r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
  new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))


#Optimal Structure method:
cl1<-cluster_optimal  (net)
test<-read.csv("test.csv",head=T)

new.result<-c()
for(i in 1:dim(test)[1])
{	r1=cl1$membership[which(as.character(cl1$names)==test[i,1])]
  r2=cl1$membership[which(as.character(cl1$names)==test[i,2])]
  new.result<-c(new.result,r1==r2)
}

table(new.result, test[,3])/rowSums(table(new.result, test[,3]))
###########################################################
#plot stuff
setwd("/Users/Tony/Desktop/2015 FALL/Machine Learning/final propject/poster")
pdf("eigenplot.pdf", height = 6.8, width = 8.5, paper = "special")
par(mar=c(5.1,6.1,4.1,2.1))
set.seed(1234)
cl <- specClust(as.matrix(adj),nn=4,method="symmetric")
cl2<-specClust(as.matrix(adj),nn=4,method="none")
cl3<-specClust(as.matrix(adj),nn=4,method="random-walk")$eigenvalue
cl3<-cl3-min(cl3)

plot(1:length(cl$eigenvalue),cl$eigenvalue,pch=1, 
     xlab = "Dimension of Laplacian Matrix", 
     ylab = "Eigenvalue", xlim = c(1,25), ylim = c(0, 0.25), col = "blue",
     cex = 2, cex.lab = 2, cex.axis = 2)
#axis(2, at = c(0, 0.2), labels = c(-0.1, 0, 0.1), cex.axis = 2)
points(1:length(cl$eigenvalue),cl2$eigenvalue,pch=3,
       col = "red", cex = 2)
points(1:length(cl$eigenvalue),cl3,pch=4,
       col = "darkgreen", cex = 2)
arrows(7, 0.04, 7, 0.025, length=0.25, code = 2,lwd = 1.5, cex = 0.8)
arrows(14, 0.065, 14, 0.05, length=0.25, code = 2,lwd = 1.5, cex = 0.8)
legend("topleft", legend = c("symmetric",
                             "none",
                             "random-walk"),
       col = c("blue", "red", "darkgreen"),
       pch = c(1,3,4), bty ='n', cex = 1.5)
dev.off()





















