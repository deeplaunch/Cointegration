library(igraph)

g = erdos.renyi.game(3000,0.1)

plot(g)

clusters(g)

degree(g)

degree.distribution(g)

# plot(seq(0,5), degree.distribution(g), type = 'l')

plot(seq(0,max(degree(g))), degree.distribution(g), type = 'l')

# DAG- Directed Acyclic Graph - like a tree

x = matrix((0,2,1,0,2,0,2,0,0),3,3)



