package Encomendas;

import Auxiliares.GPS;
import Registos.Registos;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;

public abstract class Encomenda implements Serializable,Comparable<Encomenda> {

    private String codTransportadora;
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private Double peso;
    private ArrayList<LinhaEncomenda> linhaEncomenda;
    private LocalDateTime horaPedido;
    private Boolean entregue;


    /*
    Encomenda:e3013 ,   u68             ,l58,  1.9552767,              p19,Sumo garrafa 1l,5.472888,2.5973449          ,p29,Acucar,7.5829887,2.3291283,            p67,Salsa,9.587439,44.728123
    Encomenda:NºEnc,    codUtilizador,codLoja, Peso                    codProd,Nome,
     */


    /*
    Construtores
     */

    public Encomenda(String codTransportadora, String codLoja, String codEncomenda, String codUtilizador, double peso, ArrayList<LinhaEncomenda> listaEnc, LocalDateTime horaPedido, boolean entregue) {
        this.codTransportadora = codTransportadora;
        this.codEncomenda = codEncomenda;
        this.codLoja = codLoja;
        this.codUtilizador = codUtilizador;
        this.peso = peso;
        this.linhaEncomenda = new ArrayList<LinhaEncomenda>();
        for(LinhaEncomenda x : listaEnc){
            this.linhaEncomenda.add(x.clone()); }
        this.horaPedido = horaPedido;
        this.entregue = entregue;

    }

    public Encomenda(){
        this.codTransportadora = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.codEncomenda = "";
        this.peso = 0.0;
        this.linhaEncomenda = new ArrayList<LinhaEncomenda>();
        this.horaPedido = LocalDateTime.now();
        this.entregue = false;
    }


    public Encomenda(Encomenda encomendas) {
        this.codTransportadora = encomendas.getCodTransportadora();
        this.codEncomenda = encomendas.getCodEncomenda();
        this.codUtilizador = encomendas.getCodUtilizador();
        this.codLoja = encomendas.getCodLoja();
        this.peso = encomendas.getPeso();
        this.linhaEncomenda = encomendas.getLinhaEncomenda();
        this.horaPedido = encomendas.getHoraPedido();
        this.entregue = encomendas.getEntregue();
    }


    public abstract Encomenda clone();

    public abstract String toString();


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return this.codEncomenda.equals(encomenda.getCodEncomenda());

    }



 /*
 Getters e Setters
  */

    public String getCodTransportadora() {
        return codTransportadora;
    }

    public void setCodTransportadora(String codTransportadora) {
        this.codTransportadora = codTransportadora;
    }

    public ArrayList<LinhaEncomenda> getLinhaEncomenda() {
        return linhaEncomenda;
    }

    public void setLinhaEncomenda(ArrayList<LinhaEncomenda> linhaEncomenda) {
        this.linhaEncomenda = linhaEncomenda;
    }

    public String getCodEncomenda() {
        return codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getCodLoja() {
        return codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public Double getPeso() {
        return peso;
    }

    public void setPeso(Double peso){
        this.peso = peso;
    }

    public ArrayList<LinhaEncomenda> getListaEnc() {
        return linhaEncomenda;
    }

    public void setListaEnc(ArrayList<LinhaEncomenda> listaEnc) {
        this.linhaEncomenda = listaEnc;
    }

    public LocalDateTime getHoraPedido() {
        return horaPedido;
    }

    public void setHoraPedido(LocalDateTime horaPedido) {
        this.horaPedido = horaPedido;
    }

    public Boolean getEntregue() {
        return this.entregue;
    }

    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    /*
    Métodos
     */

    //Meter mais "randomness"

    public double calculaPrecoEnc(){// 
        double res = 0.0;
        for(LinhaEncomenda x : linhaEncomenda){
            res += x.precoLinha(); }
        return res;
    }


    public GPS coordenadasLoja(Registos r){
        String x = this.getCodLoja();
        GPS y = r.getCoordenadas(x);
        return y;
    }

    public GPS coordenadasUser(Registos r){
        String x = this.getCodUtilizador();
        GPS y = r.getCoordenadas(x);
        return y;
    }

    // Adiciona um produto a encomenda
    public void adicionaProduto(LinhaEncomenda l){
        this.linhaEncomenda.add(l);
    }

    // Remove um produto da encomenda
    public void removeProduto(LinhaEncomenda l){this.linhaEncomenda.remove(l);}

    // Calcula o peso total de uma encomenda
    public void pesoTotal(){
        double res = 0.0;
        for(LinhaEncomenda l: this.linhaEncomenda){
            res += l.pesoLinha();
        }
        setPeso(res);
    }

    public String toStringGrava(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getCodEncomenda()).append(",")
                .append(this.getCodUtilizador()).append(",")
                .append(this.getCodLoja()).append(",")
                .append(this.getPeso()).append(",");
        for(LinhaEncomenda l: this.linhaEncomenda){
            sb.append(l.toStringGrava()).append(",");
        }

        sb.substring(0,sb.length() -1);
        return sb.toString();

    }


    public int compareTo(Encomenda o) {
        int codigoEncThis = Integer.parseInt(this.getCodEncomenda().replace("e",""));
        int codigoEncOther= Integer.parseInt(o.getCodEncomenda().replace("e",""));

        return Integer.compare(codigoEncThis,codigoEncOther);

    }
}
