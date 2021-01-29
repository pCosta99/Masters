import Enums.Estado;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

/**
 * Classe que lida com a informação de uma PossibilidadeEntrega.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class PossibilidadeEntrega implements Serializable {
    private final Encomenda encomenda;
    private Estado atribuida;
    private final Set<String> blackList; //Código de Transportadoras rejeitadas
    private final Localizacao gpsLoja;
    private final Localizacao gpsUtilizador;
    private LocalDateTime inicio;
    private double custo;
    private final double demora;         //Demora na entrega
    private String transportador;

    /**
     * Construtor da PossibilidadeEntrega.
     */
    public PossibilidadeEntrega(){
        this.encomenda     = new Encomenda();
        this.atribuida     = Estado.ACEITE;
        this.blackList     = new HashSet<>();
        this.gpsUtilizador = new Localizacao();
        this.gpsLoja       = new Localizacao();
        this.inicio        = LocalDateTime.now();
        this.custo         = 0.0;
        this.demora        = 0.0;
        this.transportador = "n/d";
    }

    /**
     * Construtor da PossibilidadeEntrega.
     * @param e, Encomenda da PossiblidadeEntrega a construir.
     * @param gpsLoja, Localização da loja da PossibilidadeEntrega a construir.
     * @param gpsUtilizador, Localização do utilizador da PossiblidadeEntrega a construir.
     * @param demora, Demora na entrega da PossibilidadeEntrega a construir.
     */
    public PossibilidadeEntrega(Encomenda e, Localizacao gpsLoja, Localizacao gpsUtilizador, double demora){
        this.encomenda     = e.clone();
        this.atribuida     = Estado.ACEITE;
        this.blackList     = new HashSet<>();
        this.gpsUtilizador = gpsUtilizador.clone();
        this.gpsLoja       = gpsLoja.clone();
        this.inicio        = LocalDateTime.now();
        this.custo         = 0.0;
        this.demora        = demora;
        this.transportador = "n/d";
    }

    /**
     * Construtor da PossibilidadeEntrega.
     * @param pe, PossibilidadeEntrega a construir.
     */
    public PossibilidadeEntrega(PossibilidadeEntrega pe){
        this.encomenda     = pe.getEncomenda();
        this.atribuida     = pe.getAtribuida();
        this.blackList     = pe.getBlackList();
        this.gpsUtilizador = pe.getGpsUtilizador();
        this.gpsLoja       = pe.getGpsLoja();
        this.inicio        = pe.getInicio();
        this.custo         = pe.getCusto();
        this.demora        = pe.getDemora();
        this.transportador = pe.getTransportador();
    }

    /**
     * Função que retorna a Encomeda de uma PossibilidaEntrega.
     * @return Encomenda.
     */
    private Encomenda getEncomenda(){
        return this.encomenda.clone();
    }

    /**
     * Função que retorna o estado da PossibilidadeEntregar.
     * @return Estado.
     */
    public Estado getAtribuida() { return this.atribuida; }

    /**
     * Função que retorna o código do transportador.
     * @return Código do transportador.
     */
    public String getTransportador(){
        return this.transportador;
    }

    /**
     * Função que verifica se o estado é 'Aceite'.
     * @return 'true' se for.
     */
    public boolean isAceite(){
        return this.atribuida.isAceite();
    }

    /**
     * Função que verifica se o estado é 'EsperarResposta'.
     * @return 'true' se for.
     */
    public boolean isEsperarResposta(){
        return this.atribuida.isEsperarResposta();
    }

    /**
     * Função que verifica se o estado é 'AEntregar'.
     * @return 'true' se for.
     */
    public boolean isAEntregar(){
        return this.atribuida.isAEntregar();
    }

    /**
     * Função que retorna um Set com os códigos dos transportadores rejeitados.
     * @return Set.
     */
    private Set<String> getBlackList(){
        return new HashSet<>(this.blackList);
    }

    /**
     * Função que retorna a localização do utilizador.
     * @return Localização do utilizador.
     */
    public Localizacao getGpsUtilizador(){
        return this.gpsUtilizador.clone();
    }

    /**
     * Função que retorna o codigo do utilizador.
     * @return Código do utilizador.
     */
    public String getCodigoUtilizador(){
        return this.encomenda.getCodUtilizador();
    }

    /**
     * Função que retorna o codigo da encomenda.
     * @return Código da encomenda.
     */
    public String getCodigoEncomenda() {
        return this.encomenda.getCodEncomenda();
    }

    /**
     * Função que retorna o codigo da loja.
     * @return Código da loja.
     */
    public String getCodigoLoja(){
        return this.encomenda.getCodLoja();
    }

    /**
     * Função que retorna o peso da encomenda.
     * @return Peso da encomenda.
     */
    public double getPesoEncomenda(){
        return this.encomenda.getPeso();
    }

    /**
     * Função que retorna a localização da loja.
     * @return Localização da loja.
     */
    public Localizacao getGpsLoja(){
        return this.gpsLoja.clone();
    }

    /**
     *
     * @return
     */
    private LocalDateTime getInicio() { return this.inicio; }

    /**
     * Função que retorna a demora na entrega.
     * @return Demora.
     */
    public double getDemora(){
        return this.demora;
    }

    /**
     * Função que retorna o custo.
     * @return Custo.
     */
    public double getCusto(){
        return this.custo;
    }

    /**
     * Função que modifica o estado para 'Aceite'.
     */
    public void setAceite(){
        this.atribuida = Estado.ACEITE;
    }

    /**
     * Função que modifica o estado para 'AEntregar' e modifca o transportador.
     * @param transportador, Novo transportador.
     */
    public void setAEntregar(String transportador){
        this.atribuida = Estado.AENTREGAR;
        this.transportador = transportador;
    }

    /**
     * Função que modifica o estado para 'EsperaResposta' e modifica o transportador e o custo.
     * @param transportador, Novo transportador.
     * @param custo, Novo custo.
     */
    public void setEsperar(String transportador, double custo){
        this.atribuida = Estado.ESPERARESPOSTA;
        this.transportador = transportador;
        this.custo = custo;
    }

    /**
     * Função que gera um código.
     * @return Código gerado.
     */
    public int hashCode(){
        return this.encomenda.getCodEncomenda().hashCode();
    }

    /**
     * Função que compara parametros da PossibilidadeEntrega.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        PossibilidadeEntrega that = (PossibilidadeEntrega) o;
        return this.encomenda.getCodEncomenda().equals(that.getCodigoEncomenda());
    }

    /**
     * Função que cria um clone de uma PossibilidadeEntrega.
     * @return PossibilidadeEntrega clonada.
     */
    public PossibilidadeEntrega clone(){
        return new PossibilidadeEntrega(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda: ").append(this.getCodigoEncomenda()).append('\n')
          .append("Estado: ").append(this.atribuida);
        return sb.toString();
    }

    /**
     * Função que adiciona o código de um transportador à blackList.
     * @param transportador, Código do transportador a adicionar.
     */
    public void block(String transportador){
        this.blackList.add(transportador);
    }

    /**
     * Função que verifica se a blackList contem um determinado código.
     * @param codigo, Código a verificar se existe.
     * @return 'true' se existir.
     */
    public boolean isBlocked(String codigo){
        return this.blackList.contains(codigo);
    }

    /**
     * Função que inicia o parametro 'inicio'.
     */
    public void start(){
        this.inicio = LocalDateTime.now();
    }

    /**
     * Função que verifica se é uma encomenda médica.
     * @return 'true' se for.
     */
    public boolean isMedica(){
        return this.encomenda.isMedica();
    }
}