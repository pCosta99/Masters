package TrazAqui;

import java.io.Serializable;
import java.util.*;

/**
 * Classe que guarda a informação de todos os estafetas no sistema.
 */
public abstract class Estafeta implements Entrada, Serializable {
    private String cod;
    private String nome;
    private GPS localizacao;
    private double raio;
    private List<Encomenda> encomendasEntregues;
    private List<Encomenda> pedidosEncomenda;
    private int[] classificacao;
    private boolean disponivel;
    private boolean certificada;

    /**
     * Construtor vazio de um estafeta.
     */
    public Estafeta (){
        this.cod = "";
        this.nome = "";
        this.localizacao = new GPS();
        this.raio = 0;
        this.encomendasEntregues = new ArrayList<>();
        this.pedidosEncomenda = new ArrayList<>();
        this.classificacao = new int[5];
        this.disponivel= true;
        this.certificada = false;
    }

    /**
     * Construtor parametrizador de um estafeta.
     * @param cod Código de um estafeta.
     * @param nome Nome de um estafeta.
     * @param localizacao Localização de um estafeta.
     * @param raio Raio de ação de um estafeta.
     * @param encomendasEntregues List de encomendas que já foram entregues.
     * @param pedidosEncomenda List de pedidos de encomenda.
     * @param classificacao Classificação de um estafeta.
     * @param disponivel Disponibilidade de um estafeta.
     * @param certificada Diz se um estafeta está certificado para transportes médicos ou não.
     */
    public Estafeta(String cod, String nome, GPS localizacao, double raio, List<Encomenda> encomendasEntregues, List<Encomenda> pedidosEncomenda, int[] classificacao, boolean disponivel, boolean certificada) {
        this.cod = cod;
        this.nome = nome;
        setLocalizacao(localizacao);
        this.raio = raio;
        setEncomendasEntregues(encomendasEntregues);
        setPedidosEncomenda(pedidosEncomenda);
        setClassificacao(classificacao);
        this.disponivel = disponivel;
        this.certificada = certificada;
    }

    /**
     * Construtor por cópia de um estafeta.
     * @param a Estafeta que pretendemos copiar.
     */
    public Estafeta(Estafeta a){
        this.cod = a.getCod();
        this.nome = a.getNome();
        this.localizacao = a.getLocalizacao();
        this.raio = a.getRaio();
        this.encomendasEntregues = a.getEncomendasEntregues();
        this.pedidosEncomenda = a.getPedidosEncomenda();
        this.classificacao = a.getClassificacao();
        this.disponivel = a.isDisponivel();
        this.certificada = a.aceitoTransportesMedicamentos();
    }

    /**
     * Getter do código de um estafeta.
     * @return Código de um estafeta.
     */
    public String getCod(){
        return this.cod;
    }

    /**
     * Setter do código de um estafeta.
     * @param a Código que pretendemos dar ao estafeta.
     */
    public void setCod(String a){
        this.cod = a;
    }

    /**
     * Getter do nome de um estafeta.
     * @return Nome do estafeta.
     */
    public String getNome(){
        return this.nome;
    }

    /**
     * Setter do nome de um estafeta.
     * @param n Nome que pretendemos dar ao estafeta.
     */
    public void setNome(String n){
        this.nome = n;
    }

    /**
     * Getter da localização de um estafeta.
     * @return Localização de um estafeta.
     */
    public GPS getLocalizacao(){
        return this.localizacao;
    }

    /**
     * Setter da localização de um estafeta.
     * @param a Localização que pretendemos dar a um estafeta.
     */
    public void setLocalizacao(GPS a){
        this.localizacao= a;
    }

    /**
     * Getter do raio de um estafeta.
     * @return Raio do estafeta.
     */
    public double getRaio() {
        return raio;
    }

    /**
     * Setter do raio de um estafeta.
     * @param raio Raio que pretendemos dar ao estafeta.
     */
    public void setRaio(double raio) {
        this.raio = raio;
    }

    /**
     * Método que indica se um estafeta está disponível ou não.
     * @return Disponibilidade do estafeta.
     */
    public boolean isDisponivel() {
        return disponivel;
    }

    /**
     * Setter da disponibilidade de um estafeta.
     * @param disponivel Dispobilidade que pretendemos dar ao estafeta.
     */
    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    /**
     * Getter da certificação de um estafeta.
     * @return Certificação do estafeta.
     */
    public boolean isCertificada() {
        return certificada;
    }

    /**
     * Setter da certificação de um estafeta.
     * @param certificada Certificação que pretendemos dar a um estafeta.
     */
    public void setCertificada(boolean certificada) {
        this.certificada = certificada;
    }

    /**
     * Método que indica se um estafeta pode transportar medicamentos .
     * @return Boolean que indica se o estafeta pode transportar medicamentos ou não.
     */
    public boolean aceitoTransportesMedicamentos(){
        return this.certificada;
    }

    /**
     * Getter de todas as encomendas que já foram entregues pelo estafeta.
     * @return Encomendas entregues pelo estafeta.
     */
    public List<Encomenda> getEncomendasEntregues() {
        List<Encomenda> aux= new ArrayList<>();
        for(Encomenda a : this.encomendasEntregues)
            aux.add(a.clone());
        return aux;
    }

    /**
     * Setter de encomendas entregues pelo estafeta.
     * @param encomendasEntregues Lista de encomendas entregues.
     */
    public void setEncomendasEntregues(List<Encomenda> encomendasEntregues) {
        this.encomendasEntregues = new ArrayList<>();
        encomendasEntregues.forEach(l -> this.encomendasEntregues.add(l.clone()));
    }

    /**
     * Getter de pedidos de encomenda.
     * @return Pedidos de encomenda.
     */
    public List<Encomenda> getPedidosEncomenda() {
        List<Encomenda> aux= new ArrayList<>();
        for(Encomenda a: this.pedidosEncomenda)
            aux.add(a.clone());
        return aux;
    }

    /**
     * Setter dos pedidos de encomenda.
     * @param pedidosEncomenda Lista de pedidos de encomenda.
     */
    public void setPedidosEncomenda(List<Encomenda> pedidosEncomenda) {
        this.pedidosEncomenda = new ArrayList<>();
        pedidosEncomenda.forEach(l-> this.pedidosEncomenda.add(l.clone()));
    }

    /**
     * Getter da classificação do estafeta.
     * @return Classificação do estafeta.
     */
    public int[] getClassificacao() {
        int[] ret = new int[5];
        int i = 0;
        for(int x : this.classificacao) {
            ret[i++] = x;
        }
        return ret;
    }

    /**
     * Setter da classificação de um estafeta.
     * @param classificacao Nova classificação do estafeta.
     */
    public void setClassificacao(int[] classificacao) {
        int[] ret = new int[5];
        int i = 0;
        for(int x : classificacao) {
            ret[i++] = x;
        }
        this.classificacao = ret;
    }

    /**
     * Método que permite classificar um estafeta.
     * @param x Classificação que se vai dar a um estafeta.
     */
    public void classifica(int x) {
        if (x >=1 && x<=5) {
            this.classificacao[x - 1]++;
        } else UI.print("Classificacao invalida");
    }

    /**
     * Método que calcula a classificação média de um estafeta.
     * @return Classificação média do utilizador.
     */
    public double getClassMedia(){
        int aval = 1;
        int somaclas = 0;
        int clastotal = 0;
        double avg =0;
        for(int x : this.classificacao){
            somaclas += x*aval;
            aval++;
            clastotal += x;
        }
        avg = (double) somaclas/clastotal;
        return avg;
    }

    /**
     * Método que indica se uma encomenda que está nos pedidos de encomenda está dentro do raio de ação.
     * @param pedidosEncomenda Lista de pedidos de encomenda.
     * @param lojas Lojas do sistema.
     * @param estado Estado do sistema.
     * @return Lista de pedidos de encomenda que estão dentro do raio de ação.
     */
    public List<Encomenda> aceitaEncomenda(List<Encomenda> pedidosEncomenda, HashMap<String,Loja> lojas, Estado estado){
        double dist;
        for(Encomenda e: pedidosEncomenda){
            Loja l = lojas.get(e.getLoja()).clone();
            dist = l.getLocalizacao().distancia(estado.getUserPos(e.getUtilizador()));
            if (dist>raio) pedidosEncomenda.remove(e);
        }
        return pedidosEncomenda;
    }

    /**
     * Método que remove uma encomenda dos pedidos de encomenda.
     * @param cod Código da encomenda.
     */
    public void removerEncomenda(String cod) {
        for (Encomenda e : this.pedidosEncomenda) {
            if (e.getCod().equals(cod)) {
                this.addEncomendaEntregue(e);
                this.pedidosEncomenda.removeIf(enc -> enc.getCod().equals(cod));
                break;
            }
        }
    }

    /**
     * Método que muda a disponibilidade de um estafeta.
     * @return Disponibilidade alterada.
     */
    public boolean mudaDisponibilidade(){
        this.disponivel= !this.disponivel;
        return this.disponivel;
    }

    /**
     * Método que adiciona uma encomenda à lista de encomendas entregues.
     * @param a Encomenda que pretendemos adicionar.
     */
    public void addEncomendaEntregue(Encomenda a){
        this.encomendasEntregues.add(a.clone());
    }

    /**
     * Método que adiciona uma encomenda aos pedidos de encomenda.
     * @param a Encomenda que pretendemos adicionar.
     */
    public void addPedidosEncomenda(Encomenda a){
        this.pedidosEncomenda.add(a.clone());
    }

    /**
     * Método que dá clone a um estafeta.
     * @return Estafeta clonada.
     */
    @Override
    public abstract Estafeta clone();

    /**
     * Método que dá o hashcode de um estafeta.
     * @return Hashcode do estafeta.
     */
    @Override
    public int hashCode() {
        return Objects.hash(cod, nome, localizacao);
    }

    /**
     * Método que compara dois estafetas.
     * @param o Estafeta que vamos comparar.
     * @return Booleano que indica se são iguais ou não.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Estafeta estafeta = (Estafeta) o;
        return Objects.equals(cod, estafeta.cod) &&
                Objects.equals(nome, estafeta.nome) &&
                Objects.equals(localizacao, estafeta.localizacao);
    }

    /**
     * Método que converte um Estafeta numa string.
     * @return Estafeta convertida numa string.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Estafeta{");
        sb.append("cod='").append(cod).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", localizacao=").append(localizacao);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método que converte nome numa string.
     * @return Nome.
     */
    public abstract String toStringNome();


}
