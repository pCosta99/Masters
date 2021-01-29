package Model;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Set;

/**
 * Interface de uma Encomenda
 * */
public interface IEncomenda extends Serializable {
    /**
     * @param preco Double com um preço
     * */
    void setPreco(double preco);

    /**
     * @return String com o Id de Encomenda
     * */
    String getId();

    /**
     * @return String com o Id de Loja
     * */
    String getLoja();

    /**
     * @return String com o Id de Utilizador
     * */
    String getUserId();

    /**
     * @param peso Double com um peso
     * */
    void setPeso(double peso);

    /**
     * @return Set com os Ids dos Estafetas que aceitaram a encomenda
     * */
    Set<String> getEstafeta();

    /**
     * @return List com os Produtos
     * */
    ArrayList<LinhaEncomenda> getProdutos();

    /**
     *
     * @param id String com o ID da encomenda
     * */
    void setId(String id);

    /**
     *
     * @param loja  String com o ID da loja
     * */
    void setLoja(String loja);

    /**
     *
     * @param userId  String com o ID do Utilizador
     * */
    void setUserId(String userId);

    /**
     *
     * @param produtos  List com os produtos
     * */
    void setProdutos(ArrayList<LinhaEncomenda> produtos);

    /**
     * Adiciona um novo produto aos produtos da encomenda
     *
     * */
    void addProdutos(LinhaEncomenda p);

    /**
     *
     * */
    void setTempo(int tempo);

    /**
     *
     * */
    int getTempo();
}
