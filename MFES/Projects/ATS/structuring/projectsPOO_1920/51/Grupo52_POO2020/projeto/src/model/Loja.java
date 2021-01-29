package model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Loja extends Entidade implements Serializable {
    private String codLoja;
    private List<Encomenda> queue;
    private List<Encomenda> encomendas_aceites;

    /**
     * Construtor parametrizado de uma Loja.
     * Aceita como parâmetros cada componente necessária.
     */
    public Loja( String email, String password, String nome, GPS gps, int number) {
        super(email, password, nome, gps);
        this.codLoja = "l" + number;
        this.queue = new ArrayList<>();
        this.encomendas_aceites= new ArrayList<>();
    }

    /**
     * Construtor parametrizado de uma Loja.
     * Necessário para a criação de uma loja através da leitura do ficheiro de logs.
     */
    public Loja(String codLoja, String nome, GPS gps) {
        super(codLoja,nome, gps);
        this.codLoja=codLoja;
        this.queue = new ArrayList<>();
        this.encomendas_aceites= new ArrayList<>();
    }

    public Loja(Loja loja) {
        super(loja.getEmail(), loja.getPassword(), loja.getNome(), loja.getGps());
        this.codLoja = loja.getCodLoja();
        this.queue = new ArrayList<>();
        this.encomendas_aceites= new ArrayList<>();
    }


    public String getCodLoja() {
        return this.codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public List<Encomenda> getEncomendas_aceites() {
        return encomendas_aceites;
    }

    public void setEncomendas_aceites(List<Encomenda> encomendas_aceites) {
        this.encomendas_aceites = encomendas_aceites;
    }

    public List<Encomenda> getQueue() {
        return this.queue;
    }

    public void setQueue(List<Encomenda> queue) {
        this.queue = queue;
    }

    public void addToQueue (Encomenda e){
        this.queue.add(e);
    }

    public void addToAceites(int index){
        Encomenda e = this.queue.remove(index);
        this.encomendas_aceites.add(e);
    }

    public void addToAceites(Encomenda encomenda){
        this.queue.remove(encomenda);
        this.encomendas_aceites.add(encomenda);
    }

    public List<Encomenda> allEncomendas(){
        List<Encomenda> res = new ArrayList<>();
        res.addAll(getQueue());
        res.addAll(getEncomendas_aceites());
        return res;
    }

    public Loja clone() {
        return new Loja(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("\nNome: ").append(getNome());
        sb.append("\nEmail: ").append(getEmail());
        sb.append("\nLocalização: ").append(getGps());

        return sb.toString();
    }

}
