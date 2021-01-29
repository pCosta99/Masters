/**
 * Classe que lida com a informação do histórico de uma encomenda.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
import Exception.ClassificacaoInvalidaException;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class InfoHistorico implements Comparable<InfoHistorico>, Serializable {
   // Instance Variables
   private final LocalDateTime data;
   private final String        codEncomenda;
   private final String        codUtilizador;
   private final String        codLoja;
   private final String        codTransportador;
   private final double        distancia;
   private double              duracao;
   private final double        custo;
   private int                 classificacao; //1-10

   // Constructors

   /**
    * Construtor de um InfoHistorico.
    * @param codEncomenda, Código da encomenda do InfoHistorico a construir.
    * @param codUtilizador, Código do utilizador do InfoHistorico a construir.
    * @param codLoja, Código da loja do InfoHistorico a construir.
    * @param codTransportador, Código do transportador do InfoHistorico a construir.
    * @param distancia, Distancia do InfoHistorico a construir.
    * @param duracao, Duracao do InfoHistorico a construir.
    * @param custo, Custo do InfoHistorico a construir.
    */
   public InfoHistorico(String codEncomenda, String codUtilizador, String codLoja, String codTransportador, double distancia, double duracao, double custo){
      this.data             = LocalDateTime.now();
      this.codEncomenda     = codEncomenda;
      this.codUtilizador    = codUtilizador;
      this.codLoja          = codLoja;
      this.codTransportador = codTransportador;
      this.distancia        = distancia;
      this.duracao          = duracao;
      this.custo            = custo;
      this.classificacao    = 0;
   }

   /**
    * Construtor de um InfoHistorico.
    * @param info, InfoHistorico a construir.
    */
   public InfoHistorico(InfoHistorico info) {
      this.data             = info.getData();
      this.codEncomenda     = info.getCodEncomenda();
      this.codUtilizador    = info.getCodUtilizador();
      this.codLoja          = info.getCodLoja();
      this.codTransportador = info.getCodTransportador();
      this.duracao          = info.getDuracao();
      this.distancia        = info.getDistancia();
      this.custo            = info.getCusto();
      this.classificacao    = info.getClassificacao();
   }

   //Gets

   /**
    * Função que retorna o código da encomenda.
    * @return Código da encomenda.
    */
   public String getCodEncomenda() {
      return this.codEncomenda;
   }

   /**
    * Função que retorna o código do utilizador.
    * @return Código do utilizador.
    */
   public String getCodUtilizador() {
      return this.codUtilizador;
   }

   /**
    * Função que retorna o código do transportador.
    * @return Código do transportador.
    */
   public String getCodTransportador(){
      return this.codTransportador;
   }

   /**
    * Função que retorna o código da loja.
    * @return Código da loja.
    */
   public String getCodLoja() {
      return this.codLoja;
   }

   /**
    * Função que retorna a duração da entrega.
    * @return Duração da entrega.
    */
   public double getDuracao() { return this.duracao; }

   /**
    * Função que retorna a data.
    * @return Data.
    */
   public LocalDateTime getData() {
      return this.data;
   }

   /**
    * Função que retorna a distância total percorrida durante a entrega.
    * @return Distância total percorrida durante a entrega.
    */
   public double getDistancia() { return this.distancia; }

   /**
    * Função que retorna o custo da entrega.
    * @return Custo da entrega.
    */
   public double getCusto() { return this.custo; }

   /**
    * Função que retorna a classificação atribuida à entrega.
    * @return Classificação atribuida à entrega.
    */
   public int getClassificacao() { return this.classificacao; }

   /**
    * Função que adiciona tempo à duração da entrega.
    * @param duracao, Duração a adicionar.
    */
   public void addDuracao(double duracao){
      this.duracao += duracao;
   }

   //

   /**
    * Função que cria um clone de um InfoHistorico.
    * @return InfoHistorico clonado.
    */
   public InfoHistorico clone() {
      return new InfoHistorico(this);
   }

   /**
    * Função que compara parametros do InfoHistorico.
    * @param o, Objeto a comparar.
    * @return 'true' se forem iguais.
    */
   public boolean equals(Object o){
      if(o==this) return true;
      if(o==null || o.getClass() != this.getClass()) return false;
      InfoHistorico info = (InfoHistorico) o;
      return   this.codEncomenda.equals(info.getCodEncomenda())
            && this.codLoja.equals(info.getCodLoja())
            && this.codUtilizador.equals(info.getCodUtilizador())
            && this.data.equals(info.getData())
            && this.duracao == info.getDuracao()
            && this.distancia == info.getDistancia()
            && this.custo == info.getCusto()
            && this.classificacao == info.getClassificacao();
   }

   /**
    * Função que converte os parametros do InfoHistorico para string.
    * @return String com os parametros convertidos.
    */
   public String toString(){
      StringBuilder sb = new StringBuilder();
      sb.append("Encomenda: ").append(this.codEncomenda).append('\n')
        .append("Utilizador: ").append(this.codUtilizador).append('\n')
        .append("Transportador: ").append(this.codTransportador).append('\n')
        .append("Data: ").append(this.data.format(DateTimeFormatter.ofPattern("dd-LLL-yyyy"))).append('\n')
        .append("Custo: ").append(String.format("%.2f",this.custo)).append('\n')
        .append("Demora (exceto espera em Loja): ").append(this.duracao);
      return sb.toString();
   }

   //

   /**
    * Função que compara a data do InfoHistorico.
    * @param i, InfoHistorico do qual se irá retirar uma das datas a comparar.
    * @return 0 se forem iguais.
    */
   public int compareTo(InfoHistorico i) {
      return this.data.compareTo(i.getData());
   }

   /**
    * Função que verifica se já foi classificado.
    * @return 'true' se for diferente de 0.
    */
   public boolean isClassificado(){
      return this.classificacao != 0;
   }

   /**
    * Função que modifica a classificação.
    * @param classificacao, Nova classificação.
    * @throws ClassificacaoInvalidaException, Quando a classificação for inválida.
    */
   public void setClassificacao(int classificacao) throws ClassificacaoInvalidaException{
      if(classificacao < 1 || 10 < classificacao) throw new ClassificacaoInvalidaException(classificacao);
      this.classificacao = classificacao;
   }
}
