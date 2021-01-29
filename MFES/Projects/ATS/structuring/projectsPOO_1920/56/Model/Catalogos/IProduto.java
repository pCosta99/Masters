package Model.Catalogos;

public interface IProduto {

    String getCodProduto();
    void setCodProduto(String codProduto);
    String getNome();
    void setNome(String nome);
    float getPreco();
    void setPreco(float preco);

    void criaProduto(String codProduto ,String nome, float preco);

}
