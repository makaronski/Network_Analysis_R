library(igraph)

####### Importing a graph from a .csv file #######

df=read.csv("little-rock-foodweb.csv",header=TRUE) 
# Place the cursor in the line above and hit "Run".
# Now a dialog window appears where you can select
# the file that you like to import

g=graph.data.frame(df,directed=TRUE)
# Consult the "readme" file to check whether the graph 
# you are importing is a directed or an undirected graph
# and adjust the "directed" option of the "graph.data" 
# function accordingly.

#NR of Vectors
vcount(g)

#NR of edges
ecount(g)

plot(g)


#Degree Distribution
degcentg <- degree(g)
degcentg
degdistg <- degree_distribution(g)
degdistg
plot(degdistg, type="o", col="blue",
     xaxt="n", xlab="Degree",
     ylab="Relative Frequency")
axis(1)

#Cumulative Degree Distribution
degdistcumulativeg <- degree_distribution(g,
                                          cumulative=TRUE)
degdistcumulativeg
plot(degdistcumulativeg, type="o", col="red",
     xaxt="n", xlab="Degree",
     ylab="Cumulative Frequency")
axis(1)

#Nr. of Connected components
no.clusters(g)

#Max Degree
max(degree(g))

#Min Degree
min(degree(g))

#Edge Connectivity
edge_connectivity(as.undirected(g))

#Vertex Connectvity
vertex_connectivity(as.undirected(g))

#Diameter
diameter(g)

#Average Path length
mean_distance(g)

#Transitivity
transitivity(g)

#degree centrality
max(degree(g))

#Closeness Centrality
closeness(g)
closeness(g,normalized = T)

#Betweenness Centrality
betweenness(g,directed=T)
betweenness(g,normalized=T,directed=T)


#Katz Centrality
alpha.centrality(g)

#Eigenvector_Centrality
eigen_centrality(g,directed = T)

#Page rank centrality
page_rank(g,directed=T)

#######Degree centrality REMOVAL########
degcent <- centr_degree(g, mode="all")
g1 <- delete_vertices(g,V(g)$name[degcent$res==max(degcent$res)])
plot(g1)
vcount(g1)
ecount(g1)
no.clusters(g1)
max(degree(g1))
min(degree(g1))
edge_connectivity(g1)
vertex_connectivity(g1)
diameter(g1)
mean_distance(g1)
transitivity(g1)

degcent2 <- centr_degree(g1, mode="all")
g2 <- delete_vertices(g1,V(g1)$name[degcent2$res==max(degcent2$res)])
plot(g2)
vcount(g2)
ecount(g2)
no.clusters(g2)
max(degree(g2))
min(degree(g2))
edge_connectivity(g2)
vertex_connectivity(g2)
diameter(g2)
mean_distance(g2)
transitivity(g2)

degcent3 <- centr_degree(g2, mode="all")
g3 <- delete_vertices(g2,V(g2)$name[degcent3$res==max(degcent3$res)])
plot(g3)
vcount(g3)
ecount(g3)
no.clusters(g3)
max(degree(g3))
min(degree(g3))
edge_connectivity(g3)
vertex_connectivity(g3)
diameter(g3)
mean_distance(g3)
transitivity(g3)

degdistg <- degree_distribution(g3)
degdistg
plot(degdistg, type="o", col="blue",
     xaxt="n", xlab="Degree",
     ylab="Relative Frequency")
axis(1)
plot(g3)


#######Betweenness centrality REMOVAL########
betwcent <- centr_betw(g, directed = T)
g4 <- delete_vertices(g,V(g)$name[betwcent$res==max(betwcent$res)])
plot(g4)
vcount(g4)
ecount(g4)
no.clusters(g4)
max(degree(g4))
min(degree(g4))
edge_connectivity(g4)
vertex_connectivity(g4)
diameter(g4)
mean_distance(g4)
transitivity(g4)

betwcent2 <- centr_betw(g4, directed = T)
g5 <- delete_vertices(g4,V(g4)$name[betwcent2$res==max(betwcent2$res)])
plot(g5)
vcount(g5)
ecount(g5)
no.clusters(g5)
max(degree(g5))
min(degree(g5))
edge_connectivity(g5)
vertex_connectivity(g5)
diameter(g5)
mean_distance(g5)
transitivity(g5)

betwcent3 <- centr_betw(g5, directed = T)
g6 <- delete_vertices(g5,V(g5)$name[betwcent3$res==max(betwcent3$res)])
plot(g6)
vcount(g6)
ecount(g6)
no.clusters(g6)
max(degree(g6))
min(degree(g6))
edge_connectivity(as.undirected(g6))#LOOK AT IT
vertex_connectivity(g6)
diameter(g6)
mean_distance(g6)
transitivity(g6)

degdistg <- degree_distribution(g6)
degdistg
plot(degdistg, type="o", col="blue",
     xaxt="n", xlab="Degree",
     ylab="Relative Frequency")
axis(1)
plot(g6)

###########CLOSENESS CENTRALITY REMOVAL###############
clocent <- centr_clo(g)
g7 <- delete_vertices(g,V(g)$name[clocent$res==max(clocent$res)])
plot(g7)
vcount(g7)
ecount(g7)
no.clusters(g7)
max(degree(g7))
min(degree(g7))
edge_connectivity(g7)
vertex_connectivity(g7)
diameter(g7)
mean_distance(g7)
transitivity(g7)

clocent2 <- centr_clo(g7)
g8 <- delete_vertices(g7,V(g7)$name[clocent2$res==max(clocent2$res)])
plot(g8)
vcount(g8)
ecount(g8)
no.clusters(g8)
max(degree(g8))
min(degree(g8))
edge_connectivity(g8)
vertex_connectivity(g8)
diameter(g8)
mean_distance(g8)
transitivity(g8)

clocent3 <- centr_clo(g8)
g9 <- delete_vertices(g8,V(g8)$name[clocent3$res==max(clocent3$res)])
plot(g9)
vcount(g9)
ecount(g9)
no.clusters(g9)
max(degree(g9))
min(degree(g9))
edge_connectivity(g9)
vertex_connectivity(g9)
diameter(g9)
mean_distance(g9)
transitivity(g9)

degdistg <- degree_distribution(g9)
degdistg
plot(degdistg, type="o", col="blue",
     xaxt="n", xlab="Degree",
     ylab="Relative Frequency")
axis(1)
plot(g9)
###############EIGENVECTOR CENTRALITY REMOVAL##############
evcent <- centr_eigen(g,directed = TRUE)
g10 <- delete_vertices(g,V(g)$name[evcent$vector==max(evcent$vector)])
plot(g10)
vcount(g10)
ecount(g10)
no.clusters(g10)
max(degree(g10))
min(degree(g10))
edge_connectivity(g10)
vertex_connectivity(g10)
diameter(g10)
mean_distance(g10)
transitivity(g10)

evcent2 <- centr_eigen(g,directed = T)
g11 <- delete_vertices(g10,V(g10)$name[evcent2$vector==max(evcent2$vector)])
plot(g11)
vcount(g11)
ecount(g11)
no.clusters(g11)
max(degree(g11))
min(degree(g11))
edge_connectivity(g11)
vertex_connectivity(g11)
diameter(g11)
mean_distance(g11)
transitivity(g11)

evcent3 <- centr_eigen(g,directed = T)
g12 <- delete_vertices(g11,V(g11)$name[evcent3$vector==max(evcent3$vector)])
plot(g12)
vcount(g12)
ecount(g12)
no.clusters(g12)
max(degree(g12))
min(degree(g12))
edge_connectivity(g12)
vertex_connectivity(g12)
diameter(g12)
mean_distance(g12)
transitivity(g12)

degdistg <- degree_distribution(g12)
degdistg
plot(degdistg, type="o", col="blue",
     xaxt="n", xlab="Degree",
     ylab="Relative Frequency")
axis(1)
plot(g12)

###############KATZ CENTRALITY#####################
katzcent1 <- alpha_centrality(g)
g13 <- delete_vertices(g,V(g)$name[katzcent1==max(katzcent1)])
plot(g13)
vcount(g13)
ecount(g13)
no.clusters(g13)
max(degree(g13))
min(degree(g13))
edge_connectivity(g13)
vertex_connectivity(g13)
diameter(g13)
mean_distance(g13)
transitivity(g13)

katzcent2 <- alpha_centrality(g13)
g14 <- delete_vertices(g13,V(g13)$name[katzcent2==max(katzcent2)])
plot(g14)
vcount(g14)
ecount(g14)
no.clusters(g14)
max(degree(g14))
min(degree(g14))
edge_connectivity(g14)
vertex_connectivity(g14)
diameter(g14)
mean_distance(g14)
transitivity(g14)

katzcent3 <- alpha_centrality(g14)
g15 <- delete_vertices(g14,V(g14)$name[katzcent3==max(katzcent3)])
plot(g15)
vcount(g15)
ecount(g15)
no.clusters(g15)
max(degree(g15))
min(degree(g15))
edge_connectivity(g15)
vertex_connectivity(g15)
diameter(g15)
mean_distance(g15)
transitivity(g15)

degdistg <- degree_distribution(g15)
degdistg
plot(degdistg, type="o", col="blue",
     xaxt="n", xlab="Degree",
     ylab="Relative Frequency")
axis(1)
plot(g15)

#############PAGE RANK REMOVAL#################
prcent <- page_rank(g, directed=T)
g16 <- delete_vertices(g,V(g)$name[prcent$vector==max(prcent$vector)])
plot(g16)
vcount(g16)
ecount(g16)
no.clusters(g16)
max(degree(g16))
min(degree(g16))
edge_connectivity(g16)
vertex_connectivity(g16)
diameter(g16)
mean_distance(g16)
transitivity(g16)

prcent2 <- page_rank(g16, directed=T)
g17 <- delete_vertices(g16,V(g16)$name[prcent2$vector==max(prcent2$vector)])
plot(g17)
vcount(g17)
ecount(g17)
no.clusters(g17)
max(degree(g17))
min(degree(g17))
edge_connectivity(g17)
vertex_connectivity(g17)
diameter(g17)
mean_distance(g17)
transitivity(g17)

prcent3 <- page_rank(g17, directed=T)
g18 <- delete_vertices(g17,V(g17)$name[prcent3$vector==max(prcent3$vector)])
plot(g18)
vcount(g18)
ecount(g18)
no.clusters(g18)
max(degree(g18))
min(degree(g18))
edge_connectivity(g18)
vertex_connectivity(g18)
diameter(g18)
mean_distance(g18)
transitivity(g18)

degdistg <- degree_distribution(g18)
degdistg
plot(degdistg, type="o", col="blue",
     xaxt="n", xlab="Degree",
     ylab="Relative Frequency")
axis(1)
plot(g18)


