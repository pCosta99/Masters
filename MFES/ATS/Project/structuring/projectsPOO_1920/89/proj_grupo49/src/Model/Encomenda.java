package Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.ArrayList;

/**
 * Classe que guarda toda a informação relativa a uma encomenda
 */
public class Encomenda implements Serializable {
    private String codenc;
    private String coduser;
    private String codloja;
    private String transp;
    private LocalDateTime inicio;
    private LocalDateTime fim;
    private boolean medica;
    private double peso;
    private boolean aceites;
    private int classificacao;
    private boolean entregue;
    private List<LinhaEncomenda> linha;

    /**
     * Construtor sem parametros
     */
    public Encomenda() {
        this.codenc = new String();
        this.coduser = new String();
        this.codloja = new String();
        this.transp = new String();
        this.inicio = LocalDateTime.of(0,1,1,0,0);
        this.fim = LocalDateTime.of(0,1,1,0,0);
        this.peso = 0;
        this.medica = false;
        this.aceites = false;
        this.entregue = false;
        this.classificacao = -1;
        this.linha = new ArrayList<>();

    }

    /**
     * Construtor parametrizado
     * @param e     String com codigo de encomenda
     * @param u     String com codigo de utilizador
     * @param l     String com codigo de loja
     * @param y     double com peso
     * @param li    Lista de LinhasEncomenda
     */
    public Encomenda(String e, String u, String l, double y,List<LinhaEncomenda> li) {
        this.codenc = e;
        this.coduser = u;
        this.codloja = l;
        this.transp = new String();
        this.peso = y;
        this.inicio = LocalDateTime.of(0,1,1,0,0);
        this.fim = LocalDateTime.of(0,1,1,0,0);
        this.medica = false;
        this.aceites = false;
        this.entregue = false;
        this.classificacao = -1;
        this.setLinha(li);
    }

    /**
     * Construtor por cópia
     * @param e     Encomenda a copiar
     */
    public Encomenda(Encomenda e) {
        this.codenc = e.getCodenc();
        this.coduser = e.getCoduser();
        this.codloja = e.getCodloja();
        this.transp = e.getTransp();
        this.inicio = e.getDatai();
        this.fim = e.getDataf();
        this.medica = e.getMedica();
        this.peso = e.getPeso();
        this.entregue = e.getEntregue();
        this.classificacao = e.getClassificacao();
        this.setLinha(e.getLinha());
    }

    /**
     * Get da variavel codenc do objeto
     * @return      String com codigo de encomenda
     */
    public String getCodenc() {
        return this.codenc;
    }

    /**
     * Get da variavel transp do objeto
     * @return      String com codigo de transportadora/voluntario
     */
    public String getTransp(){ return this.transp;}


    /**
     * Get da variavel entregue do objeto
     * @return      boolean
     */
    public boolean getEntregue(){return this.entregue;}

    /**
     * Get da variavel medica do objeto
     * @return      boolean
     */
    public boolean getMedica() {
        return medica;
    }

    /**
     * Set da variavel medica do objeto
     * @param medica    boolean
     */
    public void setMedica(boolean medica) {
        this.medica = medica;
    }

    /**
     * Get da variavel classificação do objeto
     * @return      int com classificação
     */
    public int getClassificacao(){ return this.classificacao;}

    /**
     * Get da variavel coduser do objeto
     * @return      String com codigo de utilizador
     */
    public String getCoduser() {
        return this.coduser;
    }

    /**
     * Get da variavel codloja do objeto
     * @return      String com codigo de loja
     */
    public String getCodloja() {
        return this.codloja;
    }

    /**
     * Get da variavel peso do objeto
     * @return      double com peso
     */
    public double getPeso() {
        return this.peso;
    }

    /**
     * Get da variavel inicio do objeto
     * @return      LocalDateTime com data e hora da aceitação
     */
    public LocalDateTime getDatai(){ return this.inicio;}

    /**
     * Get da variavel fim do objeto
     * @return      LocalDateTime com data e hora da entrega
     */
    public LocalDateTime getDataf(){ return this.fim;}

    public List<LinhaEncomenda> getLinha() {
        List<LinhaEncomenda> aux = new ArrayList<>();
        for (LinhaEncomenda l : this.linha)
            aux.add(l);
        return aux;
    }

    /**
     * Set da variavel codenc do objeto
     * @param s     String com codigo de encomenda
     */
    public void setCodenc(String s) {
        this.codenc = s;
    }

    /**
     * Set da variavel inicio do objeto
     * @param d     LocalDateTime com data e hora da aceitação
     */
    public void setDatai(LocalDateTime d){ this.inicio = d;}

    /**
     * Set da variavel inicio do objeto
     * @param d     LocalDateTime com data e hora da entrega
     */

    public void setDataf(LocalDateTime d){this.fim = d;}

    /**
     * Set da variavel transp do objeto
     * @param t     String com codigo de transportadora/voluntario
     */
    public void setTransp(String t) { this.transp = t;}

    /**
     * Set da variavel coduser do objeto
     * @param n     String com codigo de utilizador
     */
    public void setCoduser(String n) {
        this.coduser = n;
    }

    /**
     * Set da variavel entregue do objeto
     * @param b     boolean
     */
    public void setEntregue(boolean b){ this.entregue = b;}

    /**
     * Set da variavel classificacao do objeto
     * @param c     int com classificacao
     */
    public void setClassificacao(int c){ this.classificacao = c;}

    /**
     * Set da variavel codloja do objeto
     * @param l     String com codigo de loja
     */
    public void setCodloja(String l) {
        this.codloja = l;
    }

    /**
     * Set da variavel peso do objeto
     * @param l     duble com peso
     */
    public void setPeso(double l) {
        this.peso = l;
    }

    /**
     * Set da variavel linha do objeto
     * @param l     Lista a copiar
     */
    public void setLinha(List<LinhaEncomenda> l) {
        this.linha = new ArrayList<>();
        for (LinhaEncomenda li : l)
            this.linha.add(li);
    }

    /**
     * Get da variavel aceites do objeto
     * @return      boolean
     */
    public boolean getAceites() {
        return this.aceites;
    }

    /**
     * Set da variavel aceites do objeto
     * @param aceites   boolean
     */
    public void setAceites(boolean aceites) {
        this.aceites = aceites;
    }

    /**
     * Método que clona este objeto
     * @return      clone do objeto
     */
    public Encomenda clone() {
        return new Encomenda(this);
    }

    /**
     * Método equals do objeto
     * @param o     Objeto a comparar
     * @return      boolean
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Encomenda u = (Encomenda) o;
        return this.codenc.equals(u.getCodenc()) &&
                this.coduser.equals(u.getCoduser()) &&
                this.codloja.equals(u.getCodloja()) &&
                this.peso == u.getPeso() &&
                this.linha.equals(u.getLinha());
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Model.Encomenda{");
        sb.append("codenc='").append(codenc).append('\'');
        sb.append(", coduser='").append(coduser).append('\'');
        sb.append(", codloja='").append(codloja).append('\'');
        sb.append(", peso=").append(peso);
        sb.append(", aceites=").append(aceites);
        sb.append(", linha=").append(linha);
        sb.append('}');
        return sb.toString();
    }

    public int compareTo(Encomenda e1){
        return this.getCodenc().compareTo(e1.getCodenc());
    }

    /**
     * Método que calcula o preço total da encomenda
     * @return
     */
    public double getPreco() {
        return this.linha.stream().mapToDouble(LinhaEncomenda::getPreco).sum();
    }

    /**
     * Método que devolve o peso de todas as linhas de uma Lista de LinhaEncomenda
     * @return      double com peso total
     */
    public double getPesoLinhas() {
        return this.linha.stream().mapToDouble(LinhaEncomenda::getPeso).sum();
    }


}