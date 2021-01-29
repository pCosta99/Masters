package Auxiliares;

import Registos.Registos;

import java.io.Serializable;
import java.time.Duration;
import java.time.LocalDateTime;

public class Viagem implements Serializable {

    /*
    Variaveis
     */

   private Duration duracao;
   private String codEncomenda;
   private Double preco;
   private LocalDateTime inicio;
   private Double distanciaPercorrida;
   private Boolean classificado;

   /*
   Construtores
    */

   public Viagem(Duration duracao, String codEncomenda, Double preco, LocalDateTime inicio, Double distanciaPercorrida, boolean classificado) {
       this.duracao = duracao;
       this.codEncomenda = codEncomenda;
       this.preco = preco;
       this.inicio = inicio;
       this.distanciaPercorrida = distanciaPercorrida;
       this.classificado = classificado;
   }

   public Viagem(Viagem viagem) {
       this.duracao = viagem.getDuracao();
       this.codEncomenda = viagem.getCodEncomenda();
       this.preco = viagem.getPreco();
       this.inicio = viagem.getInicio();
       this.distanciaPercorrida = viagem.getDistanciaPercorrida();
       this.classificado = viagem.getClassificado();
   }

    public Viagem() {
        this.duracao = Duration.ofMinutes(0);
        this.codEncomenda = "";
        this.preco = 0.0;
        this.inicio = LocalDateTime.now();
        this.distanciaPercorrida = 0.0;
        this.classificado = false;
    }

    /*
    Gets e Sets
     */

    public Duration getDuracao() {
        return duracao;
    }

    public void setDuracao(Duration duracao) {
        this.duracao = duracao;
    }

    public String getCodEncomenda() {
        return codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public Double getPreco() {
        return preco;
    }

    public void setPreco(Double preco) {
        this.preco = preco;
    }

    public LocalDateTime getInicio() {
        return inicio;
    }

    public void setInicio(LocalDateTime inicio) {
        this.inicio = inicio;
    }

    public Double getDistanciaPercorrida() {
        return distanciaPercorrida;
    }

    public void setDistanciaPercorrida(Double distanciaPercorrida) {
        this.distanciaPercorrida = distanciaPercorrida;
    }

    public Boolean getClassificado() {
        return classificado;
    }

    public void setClassificado(boolean classificar) {
        this.classificado = classificar;
    }

    /*
        Métodos
         */
    public Viagem clone(){
        return new Viagem(this);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Viagem viagem = (Viagem) o;
        return duracao.equals(viagem.duracao) &&
                codEncomenda.equals(viagem.codEncomenda) &&
                preco.equals(viagem.preco) &&
                inicio.equals(viagem.inicio) &&
                distanciaPercorrida.equals(viagem.distanciaPercorrida);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Viagem:").append("\n")
                .append(" - Código de Encomenda: ").append(codEncomenda).append("\n")
                .append(" - Início da viagem: ").append(this.inicio).append("\n")
                .append(" - Duração da viagem: ").append(this.duracao).append("\n")
                .append(" - Distância percorrida: ").append(this.distanciaPercorrida).append("\n")
                .append(" - Preço: ").append(this.preco).append("\n");
        return sb.toString();
    }

    public String buscarUtilizador(Registos r){
        return r.getEncomendas().get(this.getCodEncomenda()).getCodUtilizador();
    }

    public String buscarEncomenda(Registos r){
        return r.getEncomendas().get(this.getCodEncomenda()).toString();
    }


    public void estaClassificado(Boolean classifica){
        this.setClassificado(classifica);
    }
}
