package Modelo.Utilizadores;

import Modelo.Encomendas.Encomenda;
import Modelo.Encomendas.RegistoEncomendas;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Transportadora extends Utilizador {

      /**
       * VARIÁVEIS DE INSTÂNCIA
       */

      private double raioAcao;
      private double taxaKM;
      private int nrTrabalhadores;
      private double classificacao;
      private int nrClassificacoes;
      private boolean disponivel;
      private List<Encomenda> pedidosAceites;

      /**
       * CONSTRUTOR VAZIO
       */

      public Transportadora() {
            super();
            this.raioAcao = 0.0;
            this.taxaKM = 0.0;
            this.nrTrabalhadores = 0;
            this.classificacao = 0.0;
            this.nrClassificacoes = 0;
            this.disponivel = false;
            this.pedidosAceites = new ArrayList<>();
      }

      /**
       * CONSTRUTOR PARAMETRIZADO 1
       */

      public Transportadora(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude) {
            super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude);
            this.raioAcao = 0.0;
            this.taxaKM = 0.0;
            this.nrTrabalhadores = 0;
            this.classificacao = 0.0;
            this.nrClassificacoes = 0;
            this.disponivel = false;
            this.pedidosAceites = new ArrayList<>();
      }

      /**
       * CONSTRUTOR PARAMETRIZADO 2
       */

      public Transportadora(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude, double raioAcao, double taxaKM, int nrTrabalhadores) {
            super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude);
            this.raioAcao = raioAcao;
            this.taxaKM = taxaKM;
            this.nrTrabalhadores = nrTrabalhadores;
            this.classificacao = 0.0;
            this.nrClassificacoes = 0;
            this.disponivel = false;
            this.pedidosAceites = new ArrayList<>();
      }

      /**
       * CONSTRUTOR PARAMETRIZADO 3
       */

      public Transportadora(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, RegistoEncomendas nRegisto, double nLatitude, double nLongitude, double nRaioAcao, double nTaxaKM, int nNrTrabalhadores, int nClassificacao, int nNrClassificacoes, boolean nDisponivel, List<Encomenda> nPedidos, List<Encomenda> nPedidosAceites) {
            super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude, nPedidos, nRegisto);
            this.raioAcao = nRaioAcao;
            this.taxaKM = nTaxaKM;
            this.nrTrabalhadores = nNrTrabalhadores;
            this.classificacao = nClassificacao;
            this.nrClassificacoes = nNrClassificacoes;
            this.disponivel = nDisponivel;
            this.pedidosAceites = new ArrayList<>();
            for(Encomenda e: nPedidosAceites) {
                  this.pedidosAceites.add(e.clone());
            }
      }

      /**
       * CONSTRUTOR POR CÓPIA
       */

      public Transportadora(Transportadora nTransporadora) {
            super(nTransporadora);
            this.raioAcao = nTransporadora.getRaioAcao();
            this.taxaKM = nTransporadora.getTaxaKM();
            this.nrTrabalhadores = nTransporadora.getNrTrabalhadores();
            this.classificacao = nTransporadora.getClassificacao();
            this.nrClassificacoes = nTransporadora.getNrClassificacoes();
            this.disponivel = nTransporadora.isDisponivel();
            this.pedidosAceites = nTransporadora.getPedidosAceites();
      }

      /**
       * GETTERS
       */

      public double getRaioAcao() {
            return this.raioAcao;
      }

      public double getClassificacao() {
            return this.classificacao;
      }

      public int getNrClassificacoes() {
            return this.nrClassificacoes;
      }

      public boolean isDisponivel() {
            return this.disponivel;
      }

      public double getTaxaKM() {
            return this.taxaKM;
      }

      public int getNrTrabalhadores() {
            return this.nrTrabalhadores;
      }

      public List<Encomenda> getPedidosAceites() {
            List<Encomenda> res = new ArrayList<>();

            for(Encomenda e: this.pedidosAceites) {
                  res.add(e.clone());
            }
            return res;
      }

      /**
       * SETTERS
       */

      public void setRaioAcao(double raioAcao) {
            this.raioAcao = raioAcao;
      }

      public void setClassificacao(double classificacao) {
            this.classificacao = classificacao;
      }

      public void setNrClassificacoes(int nrClassificacoes) {
            this.nrClassificacoes = nrClassificacoes;
      }

      public void setDisponivel(boolean disponivel) {
            this.disponivel = disponivel;
      }

      public void setNrTrabalhadores(int nrTrabalhadores) {
            this.nrTrabalhadores = nrTrabalhadores;
      }

      public void setTaxaKM(double taxaKM) {
            this.taxaKM = taxaKM;
      }

      public void setPedidosAceites(List<Encomenda> nPedidosAceites) {
            this.pedidosAceites = new ArrayList<>();

            for(Encomenda e: nPedidosAceites) {
                  this.pedidosAceites.add(e.clone());
            }
      }

      /**
       * MÉTODO CLONE
       */

      public Transportadora clone() {
            return new Transportadora(this);
      }

      /**
       * MÉTODO EQUALS
       */

      public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            if (!super.equals(o)) return false;
            Transportadora that = (Transportadora) o;
            return  super.equals(that) && Double.compare(that.raioAcao, raioAcao) == 0 &&
                    Double.compare(that.taxaKM, taxaKM) == 0 &&
                    nrTrabalhadores == that.nrTrabalhadores &&
                    classificacao == that.classificacao &&
                    nrClassificacoes == that.nrClassificacoes &&
                    disponivel == that.disponivel &&
                    Objects.equals(pedidosAceites, that.pedidosAceites);
      }

      /**
       * MÉTODO TOSTRING
       */

      public String toString() {
            final StringBuffer sb = new StringBuffer();
            sb.append("Transportadora{");
            sb.append(" RaioAcao=").append(this.raioAcao);
            sb.append(", TaxaKM=").append(this.taxaKM);
            sb.append(", nrTrabalhadores=").append(this.nrTrabalhadores);
            sb.append(", Classificacao=").append(this.classificacao);
            sb.append(", nrClassificacao=").append(this.nrClassificacoes);
            sb.append(", " + super.toString());
            sb.append(", Esta disponivel=").append(this.disponivel);
            sb.append(", Pedidos Aceites=").append(this.pedidosAceites);
            sb.append('}');
            return sb.toString();
      }

      /**
       *
       */

      public void adicionarPedidoAceite(Encomenda enc) {
            this.pedidosAceites.add(enc.clone());
      }

      public void removerPedidoAceite(Encomenda enc) {
            this.pedidosAceites.remove(enc);
      }

      public void removerTrabalhador() {
            this.nrTrabalhadores--;
      }

      public void adicionarTrabalhador() {
            this.nrTrabalhadores++;
      }

      public boolean temPedidoAceite(String codEncomenda) {
            for(Encomenda e: this.pedidosAceites) {
                  if(e.getCodEncomenda().matches(codEncomenda)) {
                        return true;
                  }
            }
            return false;
      }

      public Encomenda getPedidoAceite(String codEncomenda) {
            for(Encomenda e: this.pedidosAceites) {
                  if(e.getCodEncomenda().matches(codEncomenda)) {
                        return e.clone();
                  }
            }
            return null;
      }

      public void atualizarPedidoAceite(Encomenda enc) {
            for(Encomenda e: this.pedidosAceites) {
                  if(e.getCodEncomenda().matches(enc.getCodEncomenda())) {
                        this.pedidosAceites.remove(e);
                        this.pedidosAceites.add(enc.clone());
                  }
            }
      }
}
