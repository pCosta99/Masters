package Model;

import java.io.Serializable;
import java.util.Queue;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;

/**
 * Classe que define uma loja
 */
public class Loja extends LocalCodeName implements Serializable {

    //Queue com os clientes (voluntários e transportadoras)
    private Queue<Transporte> clientes;
    //Queue com as encomendas por preparar
    private Queue<Encomenda> encs;
    //Map com as encomendas prontas
    //  Key >> Número da encomenda
    //  Value >> Encomenda
    private Map<String, Encomenda> encsProntas;
    //Indicativo da partilha ou não da informação acerca da fila de espera
    private boolean partilha;


    /**
     * Construtor da classe
     */
    public Loja(){
        super();
        this.clientes = new LinkedList<>();
        this.encs = new LinkedList<>();
        this.encsProntas = new HashMap<>();
        partilha = true;
    }

    /**
     * Construtor da classe
     * @param code O código da loja
     * @param nome O nome da loja
     * @param gps As coordenadas GPS da loja
     * @param clientes A fila com os clientes à espera de serem atendidos
     * @param encs A fila com as encomendas por preparar
     * @param encsProntas O conjuntos das encomendas já prontas
     * @param partilha A indicação de que a loja partilha ou não a informação acerca da fila de espera dos clientes
     */
    public Loja(String code, String nome, GPS gps, Queue<Transporte> clientes, Queue<Encomenda> encs, Map<String,Encomenda> encsProntas, boolean partilha){
        super(code, nome, gps);
        this.setClientes(clientes);
        this.setEncs(encs);
        this.setEncsProntas(encsProntas);
        this.partilha = partilha;
    }

    /**
     * Construtor da classe
     * @param l Uma loja
     */
    public Loja(Loja l){
        super(l);
        this.clientes = l.getClientes();
        this.encs = l.getEncs();
        this.encsProntas = l.getEncsProntas();
        this.partilha = l.getPartilha();
    }

    /**
     * Indica os clientes que estão na fila de espera
     *
     * @return Uma cópia da fila de espera
     */
    public Queue<Transporte> getClientes() {
        Queue<Transporte> copia = new LinkedList<>();
        for (Transporte t : this.clientes) {
            copia.add(t.clone());
        }
        return copia;
    }

    /**
     * Define os clientes que estão na fila de espera
     *
     * @param clientes Os clientes na fila de espera
     */
    public void setClientes(Queue<Transporte> clientes) {
        for (Transporte copia : clientes) {
            this.clientes.add(copia.clone());
        }
    }

    /**
     * Indica as encomendas já prontas
     *
     * @return Uma cópia do mapeamento das encomendas já prontas
     */
    public Map<String, Encomenda> getEncsProntas() {
        return this.encsProntas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p -> p.getValue().clone()));
    }

    /**
     * Define as encomendas já prontas
     *
     * @param encs As encomendas já prontas
     */
    public void setEncsProntas(Map<String, Encomenda> encs) {
        this.encsProntas = encs.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p -> p.getValue().clone()));
    }

    /**
     * Indica se a loja partilha o estado da fila
     *
     * @return True caso afirmativo e false caso contrário
     */
    public boolean getPartilha() {
        return this.partilha;
    }

    /**
     * Define se uma loja partilha o estado da fila
     *
     * @param partilha True caso afirmativo e false caso contrário
     */
    public void setPartilha(boolean partilha) {
        this.partilha = partilha;
    }

    /**
     * Indica se um dado objeto é igual à loja
     *
     * @param o O objeto com o qual se pretende comparar
     * @return True caso afirmativo e false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if ((o == null) || (this.getClass() != o.getClass())) return false;
        Loja t = (Loja) o;
        return super.equals(t) && this.clientes.equals(t.getClientes())
                && this.encs.equals(t.getEncs())
                && this.encsProntas.equals(t.getEncsProntas())
                && this.partilha == t.getPartilha();
    }

    /**
     * Cria um clone de uma loja
     *
     * @return Clone da loja
     */
    public Loja clone() {
        return new Loja(this);
    }

    /**
     * Indica o tamanho da fila dos clientes
     *
     * @return O tamanho da fila
     */
    public int tamanhoFila() {
        return this.clientes.size();
    }

    /**
     * Adiciona um cliente à fila
     *
     * @param t O cliente a adicionar à fila
     * @return True caso o transporte tenha sido adicionado à fila e false caso contrário
     */
    public boolean adicionaCliente(Transporte t) {
        boolean res = false;
        if(!(this.clientes.contains(t))) {
            this.clientes.add(t.clone());
            res = true;
        }
        return res;
    }

    /**
     * Retira um cliente da fila
     *
     */
    public void retiraCliente(Transporte t) {
        this.clientes.remove(t);
    }

    /**
     * Remove uma encomenda pronta
     *
     * @param nEncomenda O número da encomenda a ser retirada
     * @return A encomenda retirada
     */
    public Encomenda retiraEncomenda(String nEncomenda) {
        return this.encsProntas.remove(nEncomenda);
    }

    /**
     * Adiciona encomenda às encomendas por preparar
     *
     * @param e A encomenda que se pretende adicionar
     */
    public void addEncomenda(Encomenda e) {
        this.encs.add(e.clone());
    }

    /**
     * Indica as encomendas por preparar
     *
     * @return Uma cópia da fila das encomendas por preparar
     */
    public Queue<Encomenda> getEncs() {
        Queue<Encomenda> copia = new LinkedList<>();
        for (Encomenda t : this.encs) {
            copia.add(t.clone());
        }
        return copia;
    }

    /**
     * Define as encomendas por preparar
     *
     * @param encs As encomendas por preparar
     */
    public void setEncs(Queue<Encomenda> encs) {
        for (Encomenda copia : encs) {
            this.encs.add(copia.clone());
        }
    }


    /**
     * Adiciona uma encomenda à fila de preparação
     *
     * @param e A encomenda que se pretende adicionar à fila
     */
    public void adicionaEncomenda(Encomenda e) {
        this.encs.add(e.clone());
    }

    /**
     * Tranfere uma encomenda da fila de preparação para o Map de encomendas prontas
     * @return Encomenda pronta
     */
    public Encomenda adicionaEncPronta() {
        Encomenda e = this.encs.remove();
        this.encsProntas.put(e.getNumEnc(), e);
        return e;
    }
    /**
     * Indica o tempo de espera na loja
     * @return O tempo de espera
     */
    public double tempoDeEspera(){
        return this.tamanhoFila() * (Math.random()*4)+2;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nome: ").append(this.getNome()).append("\n")
                .append("Código: ").append(this.getCode()).append("\n")
                .append(this.getGps()).append("\n");
        return sb.toString();
    }
}
