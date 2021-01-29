package TrazAqui;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Loja implements Entrada, Serializable {
    /**
     * Variaveis de instancia
     */
    private String cod;
    private String nome;
    private GPS localizacao;
    private List<Encomenda> pedidos;

    /**
     * Construtor vazio
     */
    public Loja () {
        this.cod = "";
        this.nome = "";
        this.localizacao = new GPS();
        this.pedidos = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param cod String
     * @param nome String
     * @param localizacao GPS
     * @param p List<Encomenda>
     */
    public Loja(String cod, String nome, GPS localizacao, List<Encomenda> p) {
        this.cod = cod;
        this.nome = nome;
        this.localizacao = localizacao;
        this.setPedidos(p);
    }

    /**
     * Construtor por copia
     * @param a Loja
     */
    public Loja(Loja a) {
        this.cod = a.getCod();
        this.nome = a.getNome();
        this.localizacao = a.getLocalizacao();
        this.pedidos = a.getPedidos();
    }

    /**
     * Retorna o codigo
     * @return String
     */
    public String getCod() {
        return cod;
    }

    /**
     * Define o codigo
     * @param cod String
     */
    public void setCod(String cod) {
        this.cod = cod;
    }

    /**
     * Retorna o nome
     * @return String
     */
    public String getNome() {
        return nome;
    }

    /**
     * Define o nome
     * @param nome String
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Retorna a localizacao
     * @return GPS
     */
    public GPS getLocalizacao() {
        return localizacao;
    }

    /**
     * Define a localizacao
     * @param localizacao GPS
     */
    public void setLocalizacao(GPS localizacao) {
        this.localizacao = localizacao;
    }

    /**
     * Retorna a lista de pedidos
     * @return List<Encomenda>
     */
    public List<Encomenda> getPedidos() {
        List<Encomenda> ret = new ArrayList<>();
        for(Encomenda e: this.pedidos)
            ret.add(e.clone());
        return ret;
    }

    /**
     * Define a lista de pedidos
     * @param pedidos List<Encomenda>
     */
    public void setPedidos(List<Encomenda> pedidos) {
        this.pedidos = new ArrayList<>();
        for(Encomenda e: pedidos)
            this.pedidos.add(e.clone());
    }

    /**
     * Adiciona uma encomenda na lista de pedidos
     * @param a Encomenda
     */
    public void addPedido(Encomenda a) {
        int i=0;
        for(Encomenda enc: this.getPedidos()){
            if(enc.getCod().equals(a.getCod())) {
                UI.printCodEncJaExiste();
                i=1;
            }

        }
        if(i==0) this.pedidos.add(a.clone());
    }

    /**
     * Remove uma encomenda da lista de pedidos
     * @param cod String
     */
    public void removePedido(String cod) {
        this.pedidos.removeIf(e -> e.getCod().equals(cod));
    }

    /**
     * Compara o objeto recebido com o que chama
     * @param o Object
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja loja = (Loja) o;
        return this.cod.equals(loja.getCod()) &&
                this.nome.equals(loja.getNome()) &&
                this.localizacao.equals(loja.getLocalizacao());
    }

    /**
     * Retorna os da loja em formato string
     * @return String
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Loja {");
        sb.append("cod = ").append(cod);
        sb.append(", nome = ").append(nome);
        sb.append(", localizacao = ").append(localizacao);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Retorna uma copia da class que a chama
     * @return Loja
     */
    public Loja clone() {
        return new Loja(this);
    }

    /**
     * Retrona uma string 
     * @return String
     */
    public String toStringNome() {
        return "Loja";
    }
}
