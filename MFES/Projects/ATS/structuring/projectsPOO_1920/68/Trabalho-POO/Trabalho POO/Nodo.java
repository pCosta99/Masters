import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Nodo implements Serializable {

    private Coordenadas GPS;
    private List<Nodo> shortestPath = new LinkedList<>();
    private Double distancia = Double.MAX_VALUE;
    private Map<Nodo,Double> nodosAdjacentes = new HashMap<>();


    public Nodo(Coordenadas GPS) {
        this.GPS = GPS;
    }

    public void addDestino(Nodo destino, double distancia) {
        nodosAdjacentes.put(destino,distancia);
    }


    /*
    * Getters e Setters
     */

    public Coordenadas getGPS() {
        return GPS;
    }

    public void setGPS(Coordenadas GPS) {
        this.GPS = GPS;
    }

    public List<Nodo> getShortestPath() {
        return shortestPath;
    }

    public void setShortestPath(List<Nodo> shortestPath) {
        this.shortestPath = shortestPath;
    }

    public void setDistancia(Double distancia) {
        this.distancia = distancia;
    }

    public double getDistancia() {
        return this.distancia;
    }

    public Map<Nodo, Double> getNodosAdjacentes() {
        return nodosAdjacentes;
    }

    public void setNodosAdjacentes(Map<Nodo, Double> nodosAdjacentes) {
        this.nodosAdjacentes = nodosAdjacentes;
    }

    public double distancia(Nodo n) {
        return this.GPS.distancia(n.getGPS());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nodo: " + this.GPS);
        return sb.toString();
    }

    public Nodo clone(){
        Nodo res = new Nodo(this.GPS);
        res.distancia = this.distancia;
        res.shortestPath = this.shortestPath.stream().map(a->a.clone()).collect(Collectors.toList());
        res.nodosAdjacentes = this.nodosAdjacentes.entrySet().stream().collect(Collectors.toMap(a->a.getKey().clone(), a->a.getValue()));
        return res;
    }

}

