package Model;

import java.time.LocalDateTime;
import java.util.Set;

import Exceptions.InvalidInputException;
import Utilities.Ponto;
import Utilities.Rating;

/** Interface da classe Voluntário. */
public interface IVoluntario {

    /**
     * Devolve o id de um voluntário. 
     * @return String com o id do voluntário.
     */
    String getId();

    /**
    * Indica se o voluntário tem condicoes para transportar medicamentos.
    * @return true se o voluntário transportar medicamentos.
    */
    boolean getTransportaMed();

    /**
    * Indica se o voluntário esta disponivel para entregar uma encomenda.
    * @return true se o voluntário estiver disponível para entregar uma encomenda.
    */
    boolean getDisponivel();
    
    /**
    * Método que atualiza a classificação de um voluntário.
    * @param novaClassificacao : nova classificação atribuida ao voluntário.
    */
    void atualizaClassificacao(String utilizador, int novaClassificacao) throws InvalidInputException;

    /** 
     * Metodo que adiciona uma encomenda ao conjunto de Encomendas entregues pelo Distribuidor.
     * 
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

    /**
     * Metodo que atualiza a disponibilidade de um voluntário
     **/
    void inverteDisponivel();

    /*
    * @return o raio de uma @class Distribuidor.
    */
    double getRaio();

    /**
     * Devolve a posiçao do Voluntario
     * @return Ponto correspondente as coordenadas GPS posiçao do Entidade.
     */
    Ponto getPosicao();

    /**
   * Atualiza se o Voluntario tem condicoes para transportar medicamentos.
   */
    void setTransportaMed(boolean novoTransportaMed);

    /**
     * Indica se o voluntário entregou ou irá entregar uma dada encomenda.
     * @param enc id da encomenda.
     * @return true se sim, false se não.
     */
    boolean temEncomenda(String enc);

    /**
     * Devolve a classificação do voluntário.
     * @return classificação
     */
    Rating getClassificacao();
    
}