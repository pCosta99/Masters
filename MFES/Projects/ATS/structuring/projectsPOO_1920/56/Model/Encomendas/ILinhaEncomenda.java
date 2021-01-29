package Model.Encomendas;

import Model.Catalogos.IProduto;
import Model.Catalogos.Produto;

public interface ILinhaEncomenda {
    IProduto getProduto();
    void setProduto(IProduto prod);
    float getQuantidade();
    void setQuantidade(float quantidade);
    float getValor();
    void setValor(float valor);

    void insereLinhaEncomenda (IProduto prod, String aux3, String aux4);
}
