package TrazAqui;


import java.io.Serializable;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.stream.Collectors;
/**
 * Classe que representa uma Loja TrazAqui!
 */
public class Loja extends Login implements Serializable {
    private int queue;
    private PriorityQueue<Encomenda> pedidos;

    /**
     * Construtor vazio do objeto Loja
     */
    public Loja(){
        super();
        queue = 0;
        pedidos = new PriorityQueue<>();
    }
    /**
     * Construtor parametrizado de um objeto Loja
     * @param codUser String que é o codigo de acesso do objeto Loja
     * @param username String que representa o username do objeto loja
     * @param password String que representa a password do objeto Loja
     * @param ponto Ponto que representa a localização
     * @param queue Queue que mostra quantas encomendas estão de momento
     */
    public Loja(String codUser, String username, String password, Ponto ponto, int queue) {
        super(codUser, username, password, ponto);
        this.queue = queue;
        pedidos = new PriorityQueue<>();
    }

    /**
     * Construtor de cópia da Classe Loja
     * @param l Objeto Loja a copiar
     */
    public Loja(Loja l){
        super(l);
        this.pedidos = l.getPedidos();
        this.queue = l.getQueue();
    }

    /**
     * Método para obter a queue
     * @return queue int
     */
    public int getQueue() {
            return this.queue;
    }
    /**
     * Método para dar set da queue
     * @param queue int
     */
    public void setQueue(int queue) {
        this.queue = queue;
    }
    /**
     * Método para obter os pedidos de uma priority queue
     * @return PriorityQueue
     */
    public PriorityQueue<Encomenda> getPedidos() {
        return pedidos.stream().map(Encomenda::clone).distinct().collect(Collectors.toCollection(PriorityQueue::new));
    }
    /**
     * Método para dar set dos pedidos e este recebe uma priorityqueue
     * @param ped PriorityQueue
     */
    public void setPedidos(PriorityQueue<Encomenda> ped) {
        this.pedidos = ped.stream().map(Encomenda::clone).distinct().collect(Collectors.toCollection(PriorityQueue::new));
    }
    /**
     * Método que adiciona à PriorityQueue uma certa Encomenda enc
     * @param enc Encomenda
     */
    public void addPedido(Encomenda enc){
        this.pedidos.add(enc);
    }
    /**
     * Método que remove da PriorityQueue o primeiro elemento da queue
     * @return Encomenda
     */
    public Encomenda removePedido(){
        return this.pedidos.poll();
    }
    /**
     * Método que remove da PriorityQueue o elemento que é dado como parametro
     * @param enc Encomenda
     */
    public void removePedido(Encomenda enc){
        this.pedidos.remove(enc);
    }
    /**
     * Método clone
     */
    @Override
    public Loja clone() {
        return new Loja(this);
    }
    /**
     * Método que cria uma String de um objeto em concreto (Neste caso de Empresa)
     */
    public String toString(){
        return "LOJA\n" + super.toString();
    }

    /**
     * Método que verifica se determinada Encomenda se encontra na fila de espera de uma loja, através do seu código.
     * @param codEncomenda Código da Encomenda
     * @return Cópia da encomenda procurada, caso exista e null caso contrário
     */
    public Encomenda verificaPedido(String codEncomenda){
        return this.pedidos.stream().filter(enc -> enc.getCod().equals(codEncomenda)).map(Encomenda::clone).findFirst().orElse(null);
    }

    public boolean verificaPedidoExiste(String codEncomenda){
        return this.pedidos.stream().anyMatch(enc -> enc.getCod().equals(codEncomenda));
    }

    /**
     * Método para verificar se um certo objeto é igual a Empresa
     * @param o Objeto a comparar
     * @return true no caso de serem iguais ou falso caso contrário
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Loja loja = (Loja) o;
        return queue == loja.queue &&
                Objects.equals(pedidos, loja.pedidos);
    }

}
