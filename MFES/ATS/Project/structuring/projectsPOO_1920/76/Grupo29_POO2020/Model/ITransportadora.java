package Model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Set;

import Exceptions.InvalidInputException;
import Utilities.Ponto;
import Utilities.Rating;

/** Interface da classe Transportadora. */
public interface ITransportadora {
    /**
     * Devolve o id de uma transportadora. 
     * @return String com o id do transportadora.
     */
    String getId();


    /**
    * Método que atualiza a classificação de uma transportadora.
    * @param novaClassificacao : nova classificação atribuida ao voluntário.
    */
    void atualizaClassificacao(String utilizador, int novaClassificacao) throws InvalidInputException;

    /*
    * Método que devolve um Map em que cada data tem associada um registo dos Kms que foram
    * precorridos.
    * @return o registo de km precorridos por data de uma transportadora.
    */
    Map<LocalDateTime, Double> getRegistoKm();


    /*
    * Método que informa sobre o total de Kms que foram precorridos até à data.
    * @return os km totais precorridos por uma transportadora.
    */
    double getKmTotais();

    /**
    * Metodo que adiciona uma encomenda ao conjunto de Encomendas entregues pelo Distribuidor.
    * @param novaEncomenda encomenda que o Distribuidor aceitou entregar (e que por isso +assa a pertencer ao seu conjunto de encomendas)
    */
    void atualizaEncomendas(String novaEncomenda);

    /**
     * Devolve o conjunto de encomendas entregues pelo Voluntario.
     *
     * @return o conjunto de encomendas entregues pelo Voluntario.
     */
    Set<String> getEncomendas();

    /**
     * Metodo que adiciona um percurso aa voluntário
     **/
    void fezKm(LocalDateTime ldt, double km) throws InvalidInputException;

    /*
     * Metodo que devolve a dispovibilidade de um distribuidor para entregar encomendas
     */
    boolean getDisponivel();

    /**
     * Metodo que atualiza a disponibilidade de um voluntário
     **/
    void inverteDisponivel();

    /**
     * Devolve a posiçao da transportadora
     * @return Ponto correspondente as coordenadas GPS posiçao do Entidade.
     */
    Ponto getPosicao();

    /**
     * Metodo que devolve o preço a pagar por cada kilometro percorrido por esta transportadora
     * @return o ppkm de uma @class Transportadora.
     */
    BigDecimal getPpkm();

    /*
    * @return o raio de uma @class Distribuidor.
    */
    double getRaio();

    /**
     * Remove uma encomenda do conjunto de rejeitadas.
     * @param enc código da encomenda
     */
    void removeRejeitada(String enc);

    /**
     * Adiciona uma encomenda do conjunto de rejeitadas.
     * @param enc código da encomenda
     */
    void adicionaRejeitada(String enc);

    /**
     * Verifica se um pedido de entrega de encomenda já foi rejeitado.
     * @param enc códdigo da encomenda.
     * @return true se já foi rejeitada, false se não.
     */
    boolean existeRejeitada(String enc);

    /**
     * Indica se o voluntário entregou ou irá entregar uma dada encomenda.
     * @param enc id da encomenda.
     * @return true se sim, false se não.
     */
    boolean temEncomenda(String enc);

    /**
     * Devolve a classificação da transportadora.
     * @return classificação
     */
    Rating getClassificacao();
}