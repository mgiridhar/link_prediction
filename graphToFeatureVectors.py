from igraph import *
#from pandas import *


def method(g):
    if g is None:
        return None
    adj_mat = g.get_adjacency(type=GET_ADJACENCY_BOTH, attribute="weight", default=0)
    for i in range(0, g.vcount()):
        for j in range(i+1, g.vcount()):
            if 0 <= g.get_eid(i, j, error = False):
                adj_mat[i,j]
                pass


#g = Graph.Read_GML("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/data/dummyusershundred.gml")
#g = Graph.Read_GML("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/dummyusers.gml")
g = Graph.Read_Ncol("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/data/NAWeek1.ncol",
                names=True, weights="auto", directed=False)
print(summary(g))

#method(g)

# print len(g.get_edgelist())
# print [ i['id'] for i in g.vs ]
# print [ i for i in g.es]
# print range(0, g.vcount())
# for edge in g.es:
#     f.write(str(edge.source) + " " + str(edge.target) + "\n")