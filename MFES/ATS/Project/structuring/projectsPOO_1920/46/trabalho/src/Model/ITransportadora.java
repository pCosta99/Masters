package Model;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Interface de uma Transportadora
 * */
public interface ITransportadora extends Serializable{
    /**
     * Função que devolve String com o Nome da Transportadora
     * @return String com o Nome da Transportadora
     * */
    String getNome();

    /**
     * Função que devolve List com as avaliações da Transportadora
     * @return List com as avaliações da Transportadora
     * */
    ArrayList<Integer> getRating();

    /**
     * Função que devolve Double com o range
     * @return Double com o range
     * */
    double getRange();

    /**
     * Função que devolve Double com a localização X
     * @return Double com a localização X
     * */
    double getLocalizacaoX();

    /**
     * Função que devolve Double com a localização Y
     * @return Double com a localização Y
     * */
    double getLocalizacaoY();

    /**
     * Função que dá Set na coordenada Y
     *  @param localizacaoY Double com a localização Y
     * */
    void setLocalizacaoY(double localizacaoY);

    /**
     *  Função que dá Set na coordenada X
     *  @param localizacaoX  Double com a localização X
     * */
    void setLocalizacaoX(double localizacaoX);

    /**
     * Função que devolve String com o ID da Transportadora
     * @return String com o ID da Transportadora
     * */
    String getId();

    /**
     * Função que acrescenta ao Histórico da Transportadora
     * @param s valor a acrescentar
     * */
    void addHistorico(String s);

    /**
     * Função que acrescenta à Faturação da Transportadora
     * @param d valor a acrescentar
     * */
    void addFaturacao(Double d);

    /**
     * Função que devolve
     * @return Double com o preço total do transporte
     * */
    double getPreco_transporte();

    /**
     *  String com o range da Transportadora
     * @param range String com o range da Transportadora
     * */
    void setRange(double range);

    /**
     *  String com o ID da Transportadora
     * @param id String com o ID da Transportadora
     * */
    void setId(String id) ;

    /**
     *  Double com o preço por km da Transportadora
     * @param preco_km Double com o preço por km da Transportadora
     * */
    void setPreco_km(double preco_km);

    /**
     *  String com o NIF da Transportadora
     * @param nif String com o NIF da Transportadora
     * */
    void setNif(String nif);

    /**
     * String com o Nome da Transportadora
     * @param nome String com o Nome da Transportadora
     * */
    void setNome(String nome);

    /**
     * Função que devolve Media de ratings
     * @return Media de ratings
     * */
    Double estrela();

    /**
     * Função que devolve String com o Email da Transportadora
     * @return String com o Email da Transportadora
     * */
    String getEmail();

    /**
     * Função que devolve String com a password da Transportadora
     * @return String com a password da Transportadora
     * */
    String getPwd();

    /**
     *  String com a password da Transportadora
     * @param s String com a password da Transportadora
     * */
    void setPwd(String s);

    /**
     *  String com o email da Transportadora
     * @param s email da Transportadora
     * */
    void setEmail(String s);

    /**
     * Double com o preço de transporte da Transportadora
     * @param preco_transporte preço de transporte
     * */
    void setPreco_transporte(double preco_transporte);

    /**
     * Função que devolve Double com o preço por km da Transportadora
     * @return Double com o preço por km da Transportadora
     * */
    double getPreco_km();

    /**
     * Função que devolve True ou false consoante a disponibilidade da transportadora para entregar
     * @return True ou false consoante a disponibilidade da transportadora para entregar
     * */
    boolean check_available();

    /**
     * Função que torna a transportadora disponivel para entrgas
     * */
    void available();

    /**
     * Função que devolve Double com o total percorrido pela transportadora da Transportadora
     * @return Double com o total percorrido pela transportadora da Transportadora
     * */
    double getDistancia();

    /**
     * Double com o total percorrido pela transportadora da Transportadora
     * @param d distancia
     * */
    void setDistancia(Double d);

    /**
     * Função que devolve uma Lista de Strings correspondente ao Histórico da Transportadora
     * @return Lista de Strings correspondente ao Histórico da Transportadora
     * */
    List<String> getHistorico();

    /**
     * Função que devolve a Faturação Total da Transportadora
     * @return Faturação Total da Transportadora
     * */
    Double fat();
}
