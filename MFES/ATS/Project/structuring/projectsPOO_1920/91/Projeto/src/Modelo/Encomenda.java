
package Modelo;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Encomenda implements Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<LinhaEncomenda> linhadeEncomenda = new ArrayList<>();
    private boolean medico;
    private boolean aceite;
    private boolean entregue;
    private LocalDate dataDeEntrega ;
    private double tempoEntrega;

    /**
     * Construtor vazio
     */
    public Encomenda(){
        this.codEncomenda = "Invalid";
        this.codUtilizador = "invalid";
        this.codLoja = "Invalid";
        this.peso = 0;
        this.linhadeEncomenda = new ArrayList<>();
        this.medico = false;
        this.entregue = false;
        this.aceite = false;
        this.dataDeEntrega = LocalDate.now();
        this.tempoEntrega = 0;

    }

    /**
     * Construtor parametrizado
     * @param codEncomenda
     * @param codUtilizador
     * @param codLoja
     * @param preco
     * @param linhadeEncomenda
     * @param medico
     * @param entregue
     * @param aceite
     * @param tempoEntrega
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double preco,
                     List<LinhaEncomenda> linhadeEncomenda , boolean medico , boolean entregue , boolean aceite, double tempoEntrega) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.linhadeEncomenda = new ArrayList<>();
        for (LinhaEncomenda a : linhadeEncomenda) {
            this.linhadeEncomenda.add(a.clone());
        }
        this.medico = medico;
        this.entregue = entregue;
        this.aceite = aceite;
        this.dataDeEntrega = LocalDate.now();
        this.tempoEntrega = tempoEntrega;
    }

    /**
     * Construtor cópia
     * @param outro
     */
    public Encomenda(Encomenda outro){
        this.codEncomenda = outro.getCodEncomenda();
        this.codUtilizador = outro.getCodUtilizador();
        this.codLoja = outro.getCodLoja();
        this.peso = outro.getPeso();
        this.linhadeEncomenda = new ArrayList<>();
        for (LinhaEncomenda a:
             outro.linhadeEncomenda) {
            this.linhadeEncomenda.add(a.clone());
        }
        this.medico = outro.isMedico();
        this.entregue = outro.isEntregue();
        this.aceite = outro.aceite;
        this.dataDeEntrega = outro.getDataDeEntrega();
        this.tempoEntrega = outro.getTempoEntrega();
    }

    //SETTERS E GETTERS

    /**
     * Método que devolve o tempo de entrega
     * @return double com tempo de entrega
     */
    public double getTempoEntrega() {
        return this.tempoEntrega;
    }

    /**
     * Método que modifica o tempo de entrega
     * @param tempoEntrega
     */
    public void setTempoEntrega(double tempoEntrega) {
        this.tempoEntrega = tempoEntrega;
    }

    /**
     * Método que devolve a data de entrega
     * @return
     */
    public LocalDate getDataDeEntrega() {
        return this.dataDeEntrega;
    }

    /**
     * Método que modifica a data de entrega
     * @param dataDeEntrega
     */
    public void setDataDeEntrega(LocalDate dataDeEntrega) {
        this.dataDeEntrega = dataDeEntrega;
    }

    /**
     * Método que devolve um boolean que demonstra se uma encomenda foi aceite ou não
     * @return
     */
    public boolean isAceite() {
        return aceite;
    }

    /**
     * Método que modifica o parametro "aceite" da encomenda
     * @param aceite
     */
    public void setAceite(boolean aceite) {
        this.aceite = aceite;
    }

    /**
     * Método que devolve um boolean que diz se a encomenda já foi aceite
     * @return
     */
    public boolean isEntregue() {
        return entregue;
    }

    /**
     * Método que modifica o parametro aceite da encomenda
     * @param entregue
     */
    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    /**
     * Método que devolve True se a encomenda for medica e false se não for
     * @return
     */
    public boolean isMedico() {
        for (LinhaEncomenda l : this.linhadeEncomenda) {
            if (l.isMedico()) return true;
        } return false;
    }

    /**
     * método que modifica o parametro "medico"
     * @param medico
     */
    public void setMedico(boolean medico) {
        this.medico = medico;
    }

    /**
     * Método que devolve o codigo da encomenda
     * @return
     */
    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    /**
     * Método que modifica o codigo da encomenda
     * @param codEncomenda
     */
    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    /**
     * Método que devolve o codigo de utilizador de uma encomenda
     * @return
     */
    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    /**
     * Método que modifica o codigo de utilizador de uma encomenda
     * @param codUtilizador
     */
    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    /**
     * Método que devolve o codigo de uma loja de uma encomenda
     * @return
     */
    public String getCodLoja() {
        return this.codLoja;
    }

    /**
     * Método que modifica o codigo de uma loja
     * @param codLoja
     */
    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    /**
     * Método que devolve o peso de uma encomenda
     * @return
     */
    public double getPeso() {
        return this.peso;
    }

    /**
     * Método que modifica o peso de uma encomenda
     * @param peso
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Método que devolve uma linha de encomenda
     * @return
     */
    public List<LinhaEncomenda> getLinhadeEncomenda() {
        List<LinhaEncomenda> ret = new ArrayList<>();
        for (LinhaEncomenda a:
             this.linhadeEncomenda) {
            ret.add(a.clone());
        } return ret;
    }

    /**
     * Método que cria uma nova linha encomenda apartir de uma linha de encomenda
     * @param linhadeEncomenda
     */
    public void setLinhadeEncomenda(List<LinhaEncomenda> linhadeEncomenda) {
        this.linhadeEncomenda = new ArrayList<>();
        for (LinhaEncomenda a:
             linhadeEncomenda) {
            this.linhadeEncomenda.add(a);
        }
    }

    /**
     * Método que devolve o preço de uma encomenda
     * @return
     */
    public double precoEncomenda(){
        double preco=0;
        for (LinhaEncomenda l :
                this.linhadeEncomenda) {
            preco += l.precoLinhadeEncomenda();
        } return  preco;
    }

    /**
     * Método equals
     * @param obj
     * @return
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj==null || obj.getClass() != this.getClass()) return false;
        Encomenda a = (Encomenda) obj;
        return this.codEncomenda.equals(a.getCodEncomenda())
                && this.codUtilizador.equals(a.getCodUtilizador())
                && this.codLoja.equals(a.getCodLoja())
                && this.peso == a.getPeso()
                && this.linhadeEncomenda.equals(a.getLinhadeEncomenda())
                && this.isAceite()==a.isAceite()
                && this.isEntregue() == a.isEntregue()
                && this.isMedico() == a.isMedico()
                && this.getDataDeEntrega().equals(a.getDataDeEntrega())
                && this.getTempoEntrega() == a.tempoEntrega;
    }

    @Override
    /**
     * Método clone
     */
    public Encomenda clone() {
        return new Encomenda(this);
    }

    /**
     * Metodo toSrting
     * @return
     */
    @Override
    public String toString() {
        return "Código da encomenda: " + this.codEncomenda
                + " Código do utilizador: " + this.codUtilizador
                + " Código da loja " + this.codLoja
                + " Peso " + this.peso
                + this.linhadeEncomenda.toString() + "\n";
    }

    /**
     * Método que adiciona uma linha encomenda a uma linha encomenda
     * @param l
     */
    public void addLinhaEncomenda (LinhaEncomenda l){
        this.linhadeEncomenda.add(l);
    }

}
