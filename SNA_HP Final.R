install.packages("packcircles")
library(igraph)
conversation <- read.csv("Just Characters.csv")
y <- data.frame(conversation$Speaker, conversation$Listener)
HP_Net <- graph.data.frame(y, directed = T)
V(HP_Net)
E(HP_Net)
V(HP_Net)$label <- V(HP_Net)$name
V(HP_Net)$degree <- degree(HP_Net)
#plot(conversationGraph)
hist(V(HP_Net)$degree,
     col = "lightblue",
     main = "Histogram of Node Degree",
     ylab = "Frequency",
     xlab = "Degree of vertices")

set.seed(222)

plot(HP_Net, 
     vertex.label.color = "black",
     vertex.color = rainbow(52),
     vertex.size=V(HP_Net)$degree*0.05,
     vertex.label.cex=0.5,
     vertex.label.dist=0,
     edge.arrow.size=0.03,
     edge.width = 0.2,
     layout = layout.kamada.kawai)

HP_net_Und <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(HP_net_Und)
plot(cnet, HP_net_Und, vertex.size = 10,
     vertex.label.cex = 0.5)

n1 <- neighbors(HP_net_Und, 'Harry', mode = c('in'))
n2<- neighbors(HP_net_Und, 'Hagrid', mode = c('in'))
t1 <- table(n1$name)
t2 <- table(n2$name)
t1
t2
barplot(t1, col="steelblue", )

barplot(t1,
        main = "Interation with Harry",
        ylab = "Frequency of interaction",
        col = "steelblue",
        horiz = FALSE, las=2, cex.axis=0.8, cex.names=0.8 )

barplot(t2,
        main = "Interation with Hagrid",
        ylab = "Frequency of interaction",
        col = "steelblue",
        horiz = FALSE, las=2,cex.axis=0.8, cex.names=0.8)
