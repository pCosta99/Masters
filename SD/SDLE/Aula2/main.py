import networkx as nx
import random as r
import matplotlib.pyplot as mplot
import numpy as np
import itertools as it
import math

def calcProbs(G, size):
    probs = []
    for p in range(size):
        probs.append((G.degree[p]+1)/(size+(2*G.number_of_edges())))
    return probs

# Now generates biasing the most choosed vertices through calcProbs
def untilConnected(nVert):
    G = nx.Graph()
    G.add_nodes_from(range(nVert))

    while not nx.is_connected(G):
        probs = calcProbs(G, nVert)
        edge1 = np.random.choice(range(nVert),p=probs)
        edge2 = np.random.choice(range(nVert),p=probs)
        if not G.has_edge(edge1,edge2): G.add_edge(edge1,edge2)

    return G

def massTesting(lb, ub, step, rep, x, y):
    for size in range(lb,ub,step):
        trackWeights = [0] * size
        edgeCount = 0

        # Perfect a total of rep repetitions, doing the average of the tracked weights in the end
        for ignore in range(rep):
            G = untilConnected(size)
            edgeCount += G.number_of_edges()
            # trackWeights = np.add(trackWeights, G.degree)
            for p in range(size):
                trackWeights[p] += G.degree[p]
        trackWeights = list(map(lambda x: x / rep, trackWeights))

        # Compose a list that associates each vertice with it's previsouly counted number and sort it by that number
        # After that, group by the second element of each pair and we have our desired comparison of degrees and how many vertices match that degree
        degrees = list(zip(range(size), list(map(math.floor, trackWeights))))
        degrees.sort(key=lambda x: x[1])
        howManyDegree = [ list(v) for k, v in it.groupby(degrees, lambda x: x[1]) ]
        print(howManyDegree)
        howManyDegree = list(map(lambda x: (len(x), x[0][1]), howManyDegree))
        howManyDegree.sort(key=lambda x: x[0])

        # Save the bar plot to a pdf
        mplot.bar(range(len(howManyDegree)), list(map(lambda x:x[0], howManyDegree)))
        mplot.xticks(range(len(howManyDegree)), list(map(lambda x:x[1], howManyDegree)))
        mplot.title("Size: " + str(size))
        mplot.xlabel("Degree")
        mplot.ylabel("How many")
        mplot.savefig("Plot" + str(size) + ".pdf")
        print("Saved Plot", str(size), " to pdf!")
        mplot.clf()

        x.append(size)
        y.append(edgeCount/rep)

x = []
y = []

massTesting(15, 31, 15, 10, x, y)

mplot.plot(x,y)
mplot.xlabel('Vertices')
mplot.ylabel('Edges')
mplot.savefig("Average.pdf")
