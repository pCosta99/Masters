package Modelo.Encomendas;

import java.io.Serializable;
import java.nio.charset.Charset;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Random;

public class Encomenda implements Serializable {

     /**
      * VARIÁVEIS DE INSTÂNCIA
      */

     private String codEncomenda;
     private String codLoja;
     private String codUtilizador;
     private String codTransportadora;
     private LocalDate data;
     private LocalDateTime tempoLoja;
     private LocalDateTime tempoTransporte;
     private LocalDateTime tempoTotal;
     private double precoLoja;
     private double precoTransporte;
     private double precoTotal;
     private double pesoTotal;
     private Map<String, LinhaEncomenda> produtos;

     /**
      * CONSTRUTOR VAZIO
      */

     public Encomenda() {
          this.codEncomenda = "n/a";
          this.codLoja = "n/a";
          this.codUtilizador = "n/a";
          this.codTransportadora = "n/a";
          this.data = LocalDate.now();
          this.tempoLoja = LocalDateTime.now();
          this.tempoTransporte = LocalDateTime.now();
          this.tempoTotal = LocalDateTime.now();
          this.precoLoja = 0.0;
          this.precoTransporte = 0.0;
          this.precoTotal = 0.0;
          this.pesoTotal = 0.0;
          this.produtos = new HashMap<>();
     }

     /**
      * CONSTRUTOR PARAMETRIZADO 1
      */

     public Encomenda(String nCodLoja, String nCodUtilizador) {

          String AlphaNumericString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "0123456789" + "abcdefghijklmnopqrstuvxyz";

          StringBuilder sb = new StringBuilder(5);

          for (int i = 0; i < 5; i++) {
               int index = (int)(AlphaNumericString.length() * Math.random());
               sb.append(AlphaNumericString.charAt(index));
          }

          this.codEncomenda = sb.toString();
          this.codLoja = nCodLoja;
          this.codUtilizador = nCodUtilizador;
          this.codTransportadora = "n/a";
          this.data = LocalDate.now();
          this.tempoLoja = null; //adicionado pela loja
          this.tempoTransporte = null; //adicionado pela transportadora/voluntario
          this.tempoTotal = null; //adicionado pela transportadora/voluntario
          this.precoLoja = 0; //adicionado pela loja
          this.precoTransporte = 0; //adicionado pela transportadora/voluntario
          this.precoTotal = 0; //adicionado pela transportadora/voluntario
          this.pesoTotal = 0; //adicionado pela loja
          this.produtos = new HashMap<>();
     }

     /**
      * CONSTRUTOR PARAMETRIZADO 2
      */

     public Encomenda(String nCodEnc, String nCodLoja, String nCodUtilizador) {

          this.codEncomenda = nCodEnc;
          this.codLoja = nCodLoja;
          this.codUtilizador = nCodUtilizador;
          this.codTransportadora = "n/a";
          this.data = LocalDate.now();
          this.tempoLoja = null; //adicionado pela loja
          this.tempoTransporte = null; //adicionado pela transportadora/voluntario
          this.tempoTotal = null; //adicionado pela transportadora/voluntario
          this.precoLoja = 0; //adicionado pela loja
          this.precoTransporte = 0; //adicionado pela transportadora/voluntario
          this.precoTotal = 0; //adicionado pela transportadora/voluntario
          this.pesoTotal = 0; //adicionado pela loja
          this.produtos = new HashMap<>();
     }

     /**
      * CONSTRUTOR PARAMETRIZADO 3
      */

     public Encomenda(String nCodEncomenda, String nCodLoja, String nCodUtilizador, String nCodTransportadora, LocalDate nData, LocalDateTime nTempoLoja, LocalDateTime nTempoTransporte, LocalDateTime nTempoTotal, double nPrecoLoja, double nPrecoTransporte, double nPrecoTotal, double nPesoTotal, Map<String, LinhaEncomenda> nProdutos) {
          this.codEncomenda = nCodEncomenda;
          this.codLoja = nCodLoja;
          this.codUtilizador = nCodUtilizador;
          this.codTransportadora = nCodTransportadora;
          this.data = nData;
          this.tempoLoja = nTempoLoja;
          this.tempoTransporte = nTempoTransporte;
          this.tempoTotal = nTempoTotal;
          this.precoLoja = nPrecoLoja;
          this.precoTransporte = nPrecoTransporte;
          this.precoTotal = nPrecoTotal;
          this.pesoTotal = nPesoTotal;
          this.produtos = new HashMap<>();
          for (Map.Entry<String, LinhaEncomenda> l : nProdutos.entrySet()) {
               this.produtos.put(l.getKey(), l.getValue().clone());
          }
     }

     /**
      * CONSTRUTOR POR CÓPIA
      */

     public Encomenda(Encomenda nEncomenda) {
          this.codEncomenda = nEncomenda.getCodEncomenda();
          this.codLoja = nEncomenda.getCodLoja();
          this.codUtilizador = nEncomenda.getCodUtilizador();
          this.codTransportadora = nEncomenda.getCodTransportadora();
          this.data = nEncomenda.getData();
          this.tempoLoja = nEncomenda.getTempoLoja();
          this.tempoTransporte = nEncomenda.getTempoTransporte();
          this.tempoTotal = nEncomenda.getTempoTotal();
          this.precoLoja = nEncomenda.getPrecoLoja();
          this.precoTransporte = nEncomenda.getPrecoTransporte();
          this.precoTotal = nEncomenda.getPrecoTotal();
          this.pesoTotal = nEncomenda.getPesoTotal();
          this.produtos = nEncomenda.getProdutos();
     }

     /**
      * GETTERS
      */

     public LocalDate getData() {
          return this.data;
     }

     public String getCodEncomenda() {
          return this.codEncomenda;
     }

     public String getCodLoja() {
          return this.codLoja;
     }

     public String getCodUtilizador() {
          return this.codUtilizador;
     }

     public String getCodTransportadora() {
          return this.codTransportadora;
     }

     public LocalDateTime getTempoLoja() {
          return this.tempoLoja;
     }

     public LocalDateTime getTempoTransporte() {
          return this.tempoTransporte;
     }

     public LocalDateTime getTempoTotal() {
          return this.tempoTotal;
     }

     public double getPrecoLoja() {
          return this.precoLoja;
     }

     public double getPrecoTransporte() {
          return this.precoTransporte;
     }

     public double getPrecoTotal() {
          return this.precoTotal;
     }

     public double getPesoTotal() {
          return this.pesoTotal;
     }

     public Map<String, LinhaEncomenda> getProdutos() {
          Map<String, LinhaEncomenda> rprodutos = new HashMap<>();

          for(Map.Entry<String, LinhaEncomenda> m: this.produtos.entrySet()) {
               rprodutos.put(m.getKey(), m.getValue().clone());
          }

          return rprodutos;
     }

     /**
      * SETTERS
      */

     public void setPesoTotal(double pesoTotal) {
          this.pesoTotal = pesoTotal;
     }

     public void setPrecoLoja(double precoLoja) {
          this.precoLoja = precoLoja;
     }

     public void setPrecoTransporte(double precoTransporte) {
          this.precoTransporte = precoTransporte;
     }

     public void setTempoLoja(LocalDateTime tempoLoja) {
          this.tempoLoja = tempoLoja;
     }

     public void setTempoTotal(LocalDateTime tempoTotal) {
          this.tempoTotal = tempoTotal;
     }

     public void setTempoTransporte(LocalDateTime tempoTransporte) {
          this.tempoTransporte = tempoTransporte;
     }

     public void setData(LocalDate data) {
          this.data = data;
     }

     public void setPrecoTotal(double precoTotal) {
          this.precoTotal = precoTotal;
     }

     public void setCodEncomenda(String codEncomenda) {
          this.codEncomenda = codEncomenda;
     }

     public void setCodTransportadora(String codTransportadora) {
          this.codTransportadora = codTransportadora;
     }

     public void setCodLoja(String codLoja) {
          this.codLoja = codLoja;
     }

     public void setCodUtilizador(String codUtilizador) {
          this.codUtilizador = codUtilizador;
     }


     public void setProdutos(Map<String, LinhaEncomenda> nProdutos) {
          this.produtos = new HashMap<>();

          for(Map.Entry<String, LinhaEncomenda> l: nProdutos.entrySet()) {
               this.produtos.put(l.getKey(),l.getValue().clone());
          }
     }

     /**
      * MÉTODO CLONE
      */

     public Encomenda clone() {
          return new Encomenda(this);
     }

     /**
      * MÉTODO EQUALS
      */

     public boolean equals(Object o) {
          if (this == o) return true;
          if (o == null || getClass() != o.getClass()) return false;
          Encomenda encomenda = (Encomenda) o;
          return Double.compare(encomenda.precoLoja, precoLoja) == 0 &&
                  Double.compare(encomenda.precoTransporte, precoTransporte) == 0 &&
                  Double.compare(encomenda.precoTotal, precoTotal) == 0 &&
                  Double.compare(encomenda.pesoTotal, pesoTotal) == 0 &&
                  Objects.equals(codEncomenda, encomenda.codEncomenda) &&
                  Objects.equals(codLoja, encomenda.codLoja) &&
                  Objects.equals(codUtilizador, encomenda.codUtilizador) &&
                  Objects.equals(codTransportadora, encomenda.codTransportadora) &&
                  Objects.equals(data, encomenda.data) &&
                  Objects.equals(tempoLoja, encomenda.tempoLoja) &&
                  Objects.equals(tempoTransporte, encomenda.tempoTransporte) &&
                  Objects.equals(tempoTotal, encomenda.tempoTotal) &&
                  Objects.equals(produtos, encomenda.produtos);
     }

     /**
      * MÉTODO TOSTRING
      */

     public String toString() {
          return "Encomenda{" +
                  "codEncomenda='" + codEncomenda + '\'' +
                  ", codLoja='" + codLoja + '\'' +
                  ", codUtilizador='" + codUtilizador + '\'' +
                  ", codTransportadora='" + codTransportadora + '\'' +
                  ", data=" + data +
                  ", tempoLoja=" + tempoLoja +
                  ", tempoTransporte=" + tempoTransporte +
                  ", tempoTotal=" + tempoTotal +
                  ", precoLoja=" + precoLoja +
                  ", precoTransporte=" + precoTransporte +
                  ", precoTotal=" + precoTotal +
                  ", pesoTotal=" + pesoTotal +
                  ", produtos=" + produtos +
                  '}';
     }

     /**
      * MÉTODO ADICIONAR PRODUTO
      */

     public void adicionarLinhaEncomenda(LinhaEncomenda nLinhaEncomenda) {
          this.produtos.put(nLinhaEncomenda.getProduto().getCodProduto(),nLinhaEncomenda.clone());
     }

     public void atualizarPesoTotal(Double peso) {
          this.pesoTotal += peso;
     }

     public void atualizarPrecoLoja(Double preco) {
          this.precoLoja += preco;
     }

     public void atualizarPrecoTotal() {
          this.precoTotal = this.precoLoja + this.precoTransporte;
     }

     /**
      * MÉTODO
      */
}
