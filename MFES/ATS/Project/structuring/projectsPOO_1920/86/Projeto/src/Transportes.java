import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * Interface que representa um transporte no sistema, todos os transportes(transportadoras e voluntarios) têm estes métodos em comum.
 */
public interface Transportes {
    double velocidadeMedia = 60.0;
    GPS getCoordenadas();
    String getNome();
    String getCodigo();
    double getRaio();
    boolean isFree();
    void setEnc(Encomenda enc);
    Transportes clone();

    /**
     * Método que determina a distancia a que um transporte se encontra de uma identidade dada.
     * @param e entidade para a qual irá ser calculada a distancia.
     * @return retorna o valor da distância em double.
     */
    double distancia(Entidade e);

    /**
     * Método que adiciona uma dada encomenda ao historico do transporte.
     * @param e encomenda que irá ser adicionada ao historico.
     */
    void addToHistorico(Encomenda e);

    /**
     * Método que faz a entrega de uma encomenda.
     * @param u utilizador para o qual a encomenda irá ser entregue.
     * @param codCli código do cliente para o qual será entregue a encomenda.
     * @param l loja na qual irá ser requisitada a encomenda.
     */
    void entregaEncomenda(Utilizador u, String codCli,Loja l);

    /**
     * Método que adiciona uma dada classificação à lista de classificações do transporte.
     * @param i classificação a adicionar à lista.
     */
    void addToClassificacao(double i);

    /**
     * Método que calcula a média das classificações para o transporte atual.
     * @return retorna a medía de classificações.
     */
    double mediaClassificacao();

    /**
     * Método que adiciona uma dada distância, ao número de kilometros feitos pelo transporte.
     * @param dist distância a ser adicionada.
     * @return retorna o número total de kilometros após a soma.
     */
    double addToNKms(double dist);

    /**
     * Método que determina a lista de encomendas presentes no histórico, entre duas datas.
     * @param date data inicial para filtrar encomendas.
     * @param date2 data final para filtrar encomendas.
     * @return retorna a lista das encomendas entre as datas dadas, presentes no histórico.
     */
    List<Encomenda> historicoData(LocalDateTime date, LocalDateTime date2);

}
