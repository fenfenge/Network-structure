
library(foreign)
library(networktools)
library("qgraph")
library("dplyr")
# devtools::install_github("GiulioCostantini/EstimateGroupNetwork")
library("EstimateGroupNetwork")
library("bootnet")
library("mgm")
library("haven")
library("reshape2")
library("readr")
# devtools::install_github("cvborkulo/NetworkComparisonTest")
library("NetworkComparisonTest")
library("BayesFactor")
library("psych")
library("lavaan")
library(networktools)


# ---------------------------------------------------------------------------------------
# ---------- 2. Load & manipulate data --------------------------------------------------
# ---------------------------------------------------------------------------------------

### Load data
data1<-read.spss("E:/try.sav",to.data.frame=TRUE)


names<-
 c("A1","A2","A3","A4","B1","B2","B3","B4","C1","C2","C3","C4","C5")

###define network
network <- EBICglasso(cor(data1), n=1622, gamma=0.3)
network ## This is an adjacency matrix!

###define community_structure
community_structure <- c(rep("Depression", 6), rep("anxiety", 7))
community_structure

###plot graph
qgraph(network, groups=community_structure, layout="spring", color=c("lightgreen", "orange"))

###calculate bridge_centrality
bridge_centrality <- bridge(network, communities=community_structure)

###
plot(bridge_centrality)

plot(bridge_centrality, include="Bridge Strength")


plot(bridge_centrality, include=c("Bridge Strength", "Bridge Betweenness"), order="value")

plot(bridge_centrality, include=c("Bridge Strength", "Bridge Betweenness"), zscore=TRUE)

bridge_centrality$`Bridge Strength`
bridge_centrality$`Bridge Betweenness`
bridge_centrality$`Bridge Closeness`
bridge_centrality$`Bridge Expected Influence (1-step)`
bridge_centrality$`Bridge Expected Influence (2-step)`


bridge_strength <- bridge_centrality$`Bridge Strength`
top_bridges <- names(bridge_strength[bridge_strength>quantile(bridge_strength, probs=0.80, na.rm=TRUE)])

bridge_strength <- bridge_centrality$`Bridge Strength`
top_bridges <- names(bridge_strength[bridge_strength>quantile(bridge_strength, probs=0.80, na.rm=TRUE)])

## Now create a new community vector where bridges are their own "community"
bridge_num_w1 <- which(names(bridge_strength) %in% top_bridges)
new_communities <- vector()
for(i in 1:length(bridge_strength)) {
 if(i %in% bridge_num_w1) {
  new_communities[i] <- "Bridge"
 } else {new_communities[i] <- community_structure[i]}
}

## And now use that community vector as your "groups" in qgraph! 
qgraph(my_network, layout="spring", groups=new_communities, color=c("lightblue", "lightgreen", "orange"))