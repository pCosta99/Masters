package Users;

import Geral.GPS;
import Interfaces.Login;
import Stock.Encomenda;
import Stock.EncomendaRealizadaUtilizador;
import Stock.EncomendaRealizadaVoluntario;
import com.sun.source.tree.Tree;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class Utilizador extends User implements Login, Serializable {

    GPS gps; // Geral.GPS guardar par de coordenadas latitude,longitude;
    private String nome;
    private List<Encomenda> on_hold;
    private Map<String,Encomenda> por_aceitar;
    private List<EncomendaRealizadaUtilizador> encomendas_realizadas;
    private List<String> por_classificar;

    /**
     * Construtor por omissão.
     */
    public Utilizador() {
        super();
        this.nome = "";
        this.gps = new GPS();
        this.on_hold = new ArrayList<>();
        this.por_aceitar = new TreeMap<>();
        this.encomendas_realizadas = new ArrayList<>();
        this.por_classificar = new ArrayList<>();
    }

    /**
     * Construtor por cópia.
     * @param u UUtilizador a copiar.
     */
    public Utilizador(Utilizador u) {
        super(u);
        this.nome = u.getNome();
        this.gps = u.getGps();
        this.on_hold = u.getOnHold();
        this.por_aceitar = u.getPorAceitar();
        this.encomendas_realizadas = u.getEncomendasFeitas();
        this.por_classificar = u.getPorClassificar();
    }

    /**
     * Construtor por parâmetros.
     * @param codigo   Código de tilizador.
     * @param nome     Nome do Utilizador.
     * @param password Password do Utilizador.
     * @param gps      Localização do Utilizador.
     */
    public Utilizador(String codigo, String nome, String password, GPS gps) {
        super(codigo,password);
        this.nome = nome;
        this.gps = gps;
        this.por_aceitar = new TreeMap<>();
        this.encomendas_realizadas = new ArrayList<>();
        this.on_hold = new ArrayList<>();
        this.por_classificar = new ArrayList<>();
    }

    /**
     * Obtém a password do Utilizador
     * @return String
     */
    public String getPassword() {
        return super.getPassword();
    }

    /**
     * Definir a password do Utilizador
     * @param password Password do Utilizador
     */
    public void setPassword(String password) {
        super.setPassword(password);
    }

    /**
     * Obtém o código do Utilizador
     * @return String
     */
    public String getCodigo() {
        return super.getCode();
    }

    /**
     * Definir o Código do Utilizador.
     * @param codigo Código do Utilizador
     */
    public void setCodigo(String codigo) {
        super.setCode(codigo);
    }

    /**
     * Obtém o nome do Utilizador
     * @return String
     */
    public String getNome() {
        return nome;
    }

    /**
     * Definir o Nome do Utilizador
     * @param nome Nome do Utilizador
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Obtém o GPS do Utilizador
     * @return GPS
     */
    public GPS getGps() {
        return this.gps;
    }

    /**
     * Definir o GPS do Utilizador.
     * @param g GPS do Utilizador
     */
    public void setGps(GPS g) {
        this.gps = g;
    }

    /**
     * Método que verifica se o login de um Utilizador é correto
     * @param code que é o código inserido pelo utilizador.
     * @param pass que é a password inserida pelo utilizador.
     * @return Validação dos dados inseridos.
     */
    @Override
    public boolean checkLogin(String code, String pass) {
        return (super.getCode().compareTo(code) == 0 && super.getPassword().compareTo(pass) == 0);
    }

    /**
     * Método que devolve as encomendas por entregar de um utilizador.
     * @return Lista com as encomendas.
     */
    public List<Encomenda> getOnHold(){
        return this.on_hold.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    /**
     * Método que define a lista de encomendas por entregar de um utilizador.
     * @param a Lista com as encomendas.
     */
    public void setOnHold(List<Encomenda> a){
        a.stream().map(Encomenda::clone).forEach(v -> this.on_hold.add(v));
    }

    /**
     * Método que adiciona uma encomenda à lista de espera do utilizador.
     * @param e Encomenda feita.
     */
    public void addEncomendaOnHold(Encomenda e){
        this.on_hold.add(e);
    }

    /**
     * Método que remove uma encomenda da lista de espera do utilizador.
     * @param e Encomenda a remover.
     */
    public void removeOnHold(Encomenda e){
        this.on_hold.remove(e);
    }


    /**
     * Método que devolve as encomendas feitas por um utilizador.
     * @return List com as encomendas feitas.
     */
    public List<EncomendaRealizadaUtilizador> getEncomendasFeitas(){
        return this.encomendas_realizadas.stream().map(EncomendaRealizadaUtilizador::clone).collect(Collectors.toList());
    }

    /**
     * Método que define as encomendas feitas por um utilizador.
     * @param a List com as encomendas feitas.
     */
    public void setEncomendasFeitas(List<EncomendaRealizadaUtilizador> a){
        a.stream().map(EncomendaRealizadaUtilizador::clone).forEach(v -> this.encomendas_realizadas.add(v));
    }

    /**
     * Método que adiciona uma encomenda à lista de encomendas realizadas de um utilizador.
     * @param e Encomenda com a encomenda a adicionar.
     * @param te double com o tempo que a encomenda demorou a ser entregue.
     * @param entregador String de quem vai realizar a entrega da encomenda.rt
     */
    public void addEncomendaRealizada(Encomenda e, double te, String entregador){
        this.encomendas_realizadas.add(new EncomendaRealizadaUtilizador(e.getCodEncomenda(),e.getCodUtilizador(),e.getCodLoja(),entregador,te,e.getDataEntrega()));
    }

    /**
     * Método que devolve uma lista com encomendas à espera de serem aceites,
     * @return Lista com encomendas.
     */
    public Map<String,Encomenda> getPorAceitar(){
        TreeMap<String,Encomenda> aux = new TreeMap<>();
        this.por_aceitar.entrySet().stream().forEach(v -> aux.put(v.getKey(),v.getValue().clone()));
        return aux;
    }

    /**
     * Método que define a lista de encomendas à espera de serem aceites.
     * @param a Lista com encomendas.
     */
    public void setPorAceitar(Map<String,Encomenda> a){
        a.entrySet().stream().forEach(v -> this.por_aceitar.put(v.getKey(),v.getValue().clone()));
    }

    /**
     * Método que adiciona uma encomenda à lista de espera.
     * @param codT que é o código da transportadora.
     * @param e que é a encomenda feita.
     */
    public void addEncomendaParaAceitar(String codT,Encomenda e){
        this.por_aceitar.putIfAbsent(codT,e);
    }

    /**
     * Método que remove uma encomenda da lista de espera.
     * @param codT que é o código da transportadora que efetuou a encomenda.
     */
    public void removePorAceitar(String codT){
        this.por_aceitar.remove(codT);
    }

    /**
     * Método que devolve uma lista com os entregadores por classificar.
     * @return Lista com entregadores.
     */
    public List<String> getPorClassificar(){
        return this.por_classificar.stream().collect(Collectors.toList());
    }

    /**
     * Método que define uma lista de entregadores a classificar.
     * @param a Lista com os entregadores.
     */
    public void setPorClassificar(List<String> a ){
        this.por_classificar = new ArrayList<>();
        a.stream().forEach(v -> this.por_classificar.add(v));
    }

    /**
     * Método que adiciona um código de entregador à lista por classificar.
     * @param code Código do entregador.
     */
    public void addCodeParaClassificar(String code){
        this.por_classificar.add(code);
    }

    /**
     * Método que remove um código de entregador da lista por classificar.
     * @param code Código do entregador.
     */
    public void removeCodePorAceitar(String code){
        this.por_classificar.remove(code);
    }

    /**
     * Converte um Utilizador numa String.
     * @return String
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador:").append(super.getCode()).append(",").
                append(this.nome).append(",").append(this.gps.getLatitude()).append(",").append(this.gps.getLongitude());
        return sb.toString();
    }

    /**
     * Cria uma cópia do Utilizador
     * @return Utilizador
     */
    public Utilizador clone() {
        return new Utilizador(this);
    }

    /**
     * Verificar se um dado Objeto é igual a este Utilizaodr
     * @param o Objeto
     * @return boolean
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return Objects.equals(getCodigo(), that.getCodigo()) &&
                Objects.equals(getNome(), that.getNome()) &&
                Objects.equals(getPassword(), that.getPassword()) &&
                Objects.equals(getGps(), that.getGps());
    }

}