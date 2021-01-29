package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Interface de um Voluntário
 * */
public interface IVoluntario extends Serializable {
    /**
     * Função que devolve String com o ID do Voluntario
     * @return String com o ID do Voluntario
     * */
    String getId();


    /**
     * Função que devolve
     * @return String com a password do Voluntario
     * */
    String getPwd();


    /**
     * @param pwd String com a password do Voluntario
     * */
    void setPwd(String pwd);


    /**
     * Função que devolve
     * @return String com o email do Voluntario
     * */
    String getEmail();


    /**
     * @param email  String com o email do Voluntario
     * */
    void setEmail(String email);


    /**
     * Função que devolve
     * @return String com o nome do Voluntario
     * */
    String getNome();

    /**
     * Função que devolve
     * @return List com as avaliações do Voluntario
     * */
    ArrayList<Integer> getRating();

    /**
     * @param id  String com o ID do Voluntario
     * */
    void setId(String id);


    /**
     * @param nome String com o nome do Voluntario
     * */
    void setNome(String nome);


    /**
     * @param localizacaoX  Double com a Localização X do Voluntario
     * */
    void setLocalizacaoX(double localizacaoX);


    /**
     * @param localizacaoY  Double com a Localização Y do Voluntario
     * */
    void setLocalizacaoY(double localizacaoY);


    /**
     * @param range  Double com o range do Voluntario
     * */
    void setRange(double range);


    /**
     * Função que devolve
     * @return Double com a media das avaliações
     * */
    double estrela();

    /**
     * Função que devolve
     * @return Double com a localozação em X
     * */
    double getLocalizacaoX();

    /**
     * Função que devolve
     * @return Double com a localozação em Y
     * */
    double getLocalizacaoY();

    /**
     * Função que devolve
     * @return Double com o alcance do voluntario
     * */
    double getRange();

    /**
     * Função que torna o Voluntario não disponivel
     * */
    void not_available();


    /**
     * Função que verifica se o Voluntario esta disponivel
     * @return se o Voluntario esta disponivel
     * */
    boolean check_available();

    /**
     * Função que torna o Voluntario disponivel
     * */
    void available();

    /**
     * Numero de encomendas realizadas pelo Voluntario
     * */
    void n_encomedas();

    /**
     * Função que devolve o Histórico do Voluntário
     * @return Histórico do voluntário em Lista de Strings
     * */
    List<String> getHistorico();

    /**
     * Função que acrescenta ao Histórico do Voluntário
     * @param s valor a acrescentar
     * */
    void addHistorico(String s);
}
