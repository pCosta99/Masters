package interfaces;

import java.time.LocalDateTime;
import java.util.Map;

public interface IEncomenda {
    String getCode();
    String getComprador();
    String getVendedor();
    LocalDateTime getData();
    double getPeso();
    boolean getMedicine();
    boolean getJaTransportada();
    Map<String,IProduto> getProdutos();
    void setCode(String code);
    void setComprador(String comprador);
    void setVendedor(String vendedor);
    void setData(LocalDateTime data);
    void setPeso(double peso);
    void setMedicine(boolean medicine);
    void setJaTransportada(boolean state);
    void setProdutos(Map<String,IProduto> produtos);
    void addProduto(ILoja loja, IUser user, IProduto p);
    IEncomenda clone();
}
