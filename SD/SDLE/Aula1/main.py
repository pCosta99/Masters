import networkx as nx
import random as r
import matplotlib.pyplot as mplot

def untilConnected(nVert):
    G = nx.Graph()
    G.add_nodes_from(range(nVert))
    while not nx.is_connected(G):
        edge1 = r.randint(0,nVert)
        edge2 = r.randint(0,nVert)
        if not G.has_edge(edge1,edge2): G.add_edge(edge1,edge2)
    return G.number_of_edges()

def massTesting(lb, ub, step, rep, x, y):
    for i in range(lb,ub,step):
        accum = 0
        for ignore in range(rep):
            accum += untilConnected(i)
        x.append(i)
        y.append(accum/rep)

x = []
y = []

massTesting(100, 300, 50, 100, x, y)

print(x)
print(y)

mplot.plot(x,y)
mplot.xlabel('Vertices')
mplot.ylabel('Edges')
mplot.show()
