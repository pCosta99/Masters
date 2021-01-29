package Model;

public interface IEncomendas {

    void put(String cod, Encomenda e);

    Encomenda extraiEncomenda(String codEncomenda);

    void adicionaProdutos(String codEncomenda, String codProduto, String descricao, double quantidade, double valorUnitario);

    void criaEncomenda(String codEncomenda, String userName, String codLoja, double peso);
}
