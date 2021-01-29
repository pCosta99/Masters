import java.io.Serializable;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class Rota implements Serializable {
    private Set<Nodo> nodos = new HashSet<>(); //Conjunto de nodos


    public void addNodo(Nodo nodo) {
        nodos.add(nodo);
    }


    private static Nodo getLowestDistanceNode(Set<Nodo> unsettledNodes) {   //Dos nodos ainda não selecionados, escolhe o nodo com menor distância à origem.
        Nodo lowestDistanceNode = null;
        double lowestDistance = Double.MAX_VALUE;
        for (Nodo nodo: unsettledNodes) {
            double distanciaNodo = nodo.getDistancia();
            if (distanciaNodo < lowestDistance) {
                lowestDistance = distanciaNodo;
                lowestDistanceNode = nodo;
            }
        }
        return lowestDistanceNode;
    }


    private static void calculateMinimumDistance(Nodo nodo, Double aresta, Nodo origem) {
        Double distanciaOrigem = origem.getDistancia();
        if (distanciaOrigem + aresta < nodo.getDistancia()) {
            nodo.setDistancia(distanciaOrigem + aresta);
            LinkedList<Nodo> shortestPath = new LinkedList<Nodo>(origem.getShortestPath());
            shortestPath.add(origem);
            nodo.setShortestPath(shortestPath);
        }
    }

    public static Rota shortestPathFromSource(Rota rota, Nodo origem) {
        origem.setDistancia(0.0);

        Set<Nodo> settledNodes = new HashSet<>();
        Set<Nodo> unsettledNodes = new HashSet<>();

        unsettledNodes.add(origem);

        while (unsettledNodes.size() != 0) {
            Nodo currentNode = getLowestDistanceNode(unsettledNodes);
            unsettledNodes.remove(currentNode);
            for (Map.Entry<Nodo,Double> par: currentNode.getNodosAdjacentes().entrySet()) {
                Nodo adjacentNode = par.getKey();
                Double aresta = par.getValue();
                if (!settledNodes.contains(adjacentNode)) {
                    calculateMinimumDistance(adjacentNode, aresta, currentNode);
                    unsettledNodes.add(adjacentNode);
                }
            }
            settledNodes.add(currentNode);
        }
        return rota;
    }

    public Rota clone(){
        Rota res = new Rota();
        res.nodos = this.nodos.stream().map(a->a.clone()).collect(Collectors.toSet());
        return res;
    }

    @Override
    public String toString() {
        return "Rota{" +
                "nodos=" + nodos +
                '}';
    }

    public static void main(String[] args) {

        Coordenadas cA = new Coordenadas (0,3);
        Coordenadas cB = new Coordenadas (0,6);
        Coordenadas cC = new Coordenadas (4,0);
        Coordenadas cD = new Coordenadas (1,3);
        Coordenadas cE = new Coordenadas (3,4);
        Coordenadas cF = new Coordenadas (2,5);

        Nodo nodeA = new Nodo(cA);
        Nodo nodeB = new Nodo(cB);
        Nodo nodeC = new Nodo(cC);
        Nodo nodeD = new Nodo(cD);
        Nodo nodeE = new Nodo(cE);
        Nodo nodeF = new Nodo(cF);

        nodeA.addDestino(nodeB, nodeA.distancia(nodeB));
        nodeA.addDestino(nodeC, nodeA.distancia(nodeC));

        nodeB.addDestino(nodeD, nodeB.distancia(nodeD));
        nodeB.addDestino(nodeF, nodeB.distancia(nodeF));

        nodeC.addDestino(nodeE, nodeC.distancia(nodeE));

        nodeD.addDestino(nodeE, nodeD.distancia(nodeE));
        nodeD.addDestino(nodeF, nodeD.distancia(nodeF));

        nodeF.addDestino(nodeE, nodeF.distancia(nodeE));

        Rota rota = new Rota();

        rota.addNodo(nodeA);
        rota.addNodo(nodeB);
        rota.addNodo(nodeC);
        rota.addNodo(nodeD);
        rota.addNodo(nodeE);
        rota.addNodo(nodeF);

        rota = shortestPathFromSource(rota, nodeA);
        System.out.println(rota.toString());
    }

}
