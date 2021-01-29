package Model;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Interface de um Utilizador
 * */
public interface IUtilizador extends Serializable{
    /**
     * Função que devolve
     * @return String com o ID do Utilizador
     * */
    String getId();

    /**
     *
     * @param nome String com o nome do Utilizador
     * */
    void setNome(String nome);

    /**
     * Função que devolve
     * @return String com o nome do Utilizador
     * */
    String getNome();

    /**
     *
     * @param id  String com o ID do Utilizador
     * */
    void setId(String id);

    /**
     * Função que devolve numero de encomendas realizadas do Utilizador
     * @return Numero de encomendas realizadas do Utilizador
     * */
    int getAcessos();

    /**
     * @param estado estado a dar Set
     * */
    void setEstado(int estado);

    /**
     *
     * @param acessos Numero de encomendas realizadas do Utilizador
     * */
    void setAcessos(int acessos);

    /**
     * Função que devolve Localização X do Utilizador
     * @return Localização X do Utilizador
     * */
    double getLocalizacaoX();

    /**
     * Função que devolve Localização Y do Utilizador
     * @return Localização Y do Utilizador
     * */
    double getLocalizacaoY();

    /**
     * Função que dá Set na coordenada X
     * @param localizacaoX  Localização X do Utilizador
     * */
    void setLocalizacaoX(double localizacaoX);

    /**
     * Função que dá Set na coordenada Y
     * @param localizacaoY Localização Y do Utilizador
     * */
    void setLocalizacaoY(double localizacaoY);

    /**
     * Função que devolve
     * @return Set de encomendas realizadas pelo Utilizador
     * */
    List<String> historico();

    /**
     * Função que devolve
     * @return String email do Utilizador
     * */
    String getEmail();

    /**
     * Função que devolve
     * @return String password do Utilizador
     * */
    String getPwd();

    /**
     * @param s String password do Utilizador
     * */
    void setPwd(String s);

    /**
     * @param s String email do Utilizador
     * */
    void setEmail(String s);

    /**
     * Função que acrescenta ao Histórico do Utilizador
     * @param enc valor a acrescentar
     * */
    void addHistorico(IEncomenda enc);
}
