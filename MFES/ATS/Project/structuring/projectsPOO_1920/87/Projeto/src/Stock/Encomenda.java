package Stock;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

public class Encomenda implements Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<LinhaEncomenda> produtos;
    private LocalDateTime start;
    private LocalDateTime end;

    /**
     * Cosntrutor por omissão.
     */
    public Encomenda() {
        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.produtos = new ArrayList<>();
        this.start = null;
        this.end = null;
    }

    /**
     * Construtor por parâmetros.
     * @param codEncomenda Código de encomenda.
     * @param codUtilizador Código de utilizador.
     * @param codLoja Código de loja.
     * @param peso Peso total.
     * @param produtos Produtos comprados.
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, Double peso, List<LinhaEncomenda> produtos) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        setProdutos(produtos);
        this.start = LocalDateTime.now();
        this.end = null;
    }

    /**
     * Construtor por cópia.
     * @param e Encomenda a ser copiada.
     */
    public Encomenda(Encomenda e) {
        this.codEncomenda = e.getCodEncomenda();
        this.codUtilizador = e.getCodUtilizador();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.produtos = e.getProdutos();
        this.start = e.getDataEncomenda();
        this.end = e.getDataEntrega();
    }

    /**
     * Método que devolve o código de uma encomenda.
     * @return Código de encomenda.
     */
    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    /**
     * Método que define o código de encomenda.
     * @param codEncomenda Código de encomenda.
     */
    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    /**
     * Método que devolve o código de utilizador que fez a encomenda.
     * @return Código de utilizador.
     */
    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    /**
     * Método que define o código de utilizador que fez a encomenda.
     * @param codUtilizador Código de utilizador.
     */
    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    /**
     * Método que devolve o código de loja onde foi feita a encomenda.
     * @return Código de loja.
     */
    public String getCodLoja() {
        return this.codLoja;
    }

    /**
     * Método que define o código de loja onde foi feita a encomenda.
     * @param codLoja Código da loja.
     */
    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    /**
     * Método que devolve o peso da encomenda.
     * @return Peso.
     */
    public double getPeso() {
        return this.peso;
    }

    /**
     * Método que define o peso de uma encomenda.
     * @param peso Peso.
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Método que devolva uma lista com os produtos comprados numa encomenda.
     * @return Lista com os produtos comprados.
     */
    public List<LinhaEncomenda> getProdutos() {
        ArrayList<LinhaEncomenda> aux = new ArrayList<>();
        for( LinhaEncomenda l : this.produtos)
            aux.add(l.clone());
        return aux;
    }

    /**
     * Método que define os produtos comprados numa encomenda.
     * @param produtos Produtos que foram comprados.
     */
    public void setProdutos(List<LinhaEncomenda> produtos) {
        this.produtos = new ArrayList<>();
        for(LinhaEncomenda l : produtos)
            this.produtos.add(l);

    }

    /**
     * Método que devolve a data em que a encomenda foi feita.
     * @return Data de encomenda.
     */
    public LocalDateTime getDataEncomenda() {
        return start;
    }

    /**
     * Método que define a data em que foi feita uma encomenda.
     * @param start Data de encomenda.
     */
    public void setDataEncomenda(LocalDateTime start) {
        this.start = start;
    }

    /**
     * Método que devolve a data de entrega de uma encomenda.
     * @return Data de entrega.
     */
    public LocalDateTime getDataEntrega() {
        return end;
    }

    /**
     * Método que define a data de entrega de uma encomenda.
     * @param end Data de entrega.
     */
    public void setDataEntrega(LocalDateTime end) {
        this.end = end;
    }

    /**
     * Método que converte numa String a informação sobre uma encomenda.
     * @return String com a informação.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda:").append(this.codEncomenda).append(",").append(this.codUtilizador).append(",").append(this.codLoja).append(",").append(this.peso);
        for(LinhaEncomenda l : this.produtos)
            sb.append(",").append(l.toString());
        return sb.toString();
    }

    /**
     * Método que averigua se um dado objeto é igual a uma dada Encomenda.
     * @param o Objeto a ser comparado.
     * @return Resultado da comparação.
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Encomenda e = (Encomenda) o;
        return (e.getCodEncomenda().equals(this.codEncomenda) &&
                e.getCodUtilizador().equals(this.codUtilizador) &&
                e.getCodLoja().equals(this.codLoja) &&
                e.getPeso() == (this.peso) &&
                e.getProdutos().equals(this.produtos));
    }

    /**
     * Método que devolve uma cópia de uma Encomenda.
     * @return Cópia.
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }


}