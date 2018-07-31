library(igraph)
library("ggplot2")
# Gnp graph 
gnp <- sample_gnp(n=500, p=0.015)
no.clusters(gnp)
is_connected(gnp)
# Small world graph
swg <- sample_smallworld(dim=1, size=500, nei=5, p=0.025)
no.clusters(swg)
is_connected(swg)
# Preferential attachment graph
pag <- sample_pa(n=500, m=3, directed=FALSE)
no.clusters(pag)
is_connected(pag)


gsize(gnp) #number of edges
deg_distr_gnp <- degree_distribution(gnp)
plot(deg_distr_gnp, type="o", col="red",
     xaxt="n", xlab="Degree",
     ylab="Cumulative Frequency")
axis(1, at=1:21, labels=0:20)

deg_distr_gnp_cum <- degree.distribution(gnp, cumulative = TRUE)
plot(deg_distr_gnp_cum, pch=19, cex=1,
     xaxt="n", col="orange", xlab="Degree",
     ylab="Cumulative Frequency")
axis(1, at=1:21, labels=0:20)
lines(deg_distr_gnp_cum, col="blue")

degree(gnp, normalized=TRUE)
diameter(gnp)
max(degree(gnp))
min(degree(gnp))
transitivity(gnp)
mean_distance(gnp)#average path length
vertex_connectivity(gnp)
edge_connectivity(gnp)


gsize(swg)
deg_distr_swg <- degree_distribution(swg)
plot(deg_distr_swg, type="o", col="red",
     xaxt="n", xlab="Degree",
     ylab="Cumulative Frequency")
axis(1, at=1:21, labels=0:20)

deg_distr_swg_cum <- degree.distribution(swg, cumulative = TRUE)
plot(deg_distr_swg_cum, pch=19, cex=1,
     xaxt="n", col="orange", xlab="Degree",
     ylab="Cumulative Frequency")
axis(1, at=1:21, labels=0:20)
lines(deg_distr_swg_cum, col="blue")


diameter(swg)
max(degree(swg))
min(degree(swg))
transitivity(swg)
mean_distance(swg)#average path length
vertex_connectivity(swg)
edge_connectivity(swg)



gsize(pag)
deg_distr_pag <- degree_distribution(pag)
plot(deg_distr_pag, type="o", col="red",
     xaxt="n", xlab="Degree",
     ylab="Cumulative Frequency")
axis(1, at=1:101, labels=0:100)

deg_distr_pag_cum <- degree.distribution(pag, cumulative = TRUE)
plot(deg_distr_pag_cum, pch=19, cex=1,
     xaxt="n", col="orange", xlab="Degree",
     ylab="Cumulative Frequency")
axis(1, at=1:101, labels=0:100)
lines(deg_distr_pag_cum, col="blue")


diameter(pag)
max(degree(pag))
min(degree(pag))
transitivity(pag)
mean_distance(pag)#average path length
vertex_connectivity(pag)
edge_connectivity(pag)
