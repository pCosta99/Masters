package interfaces;

import java.util.Map;

public interface ILoja {
    String getCode();
    String getNome();
    boolean hasFila();
    int getFilaSize();
    double getLatitude();
    double getLongitude();
    Map<String, IProduto> getProdutos();
    void setCode(String code);
    void setNome(String nome);
    void setFila(boolean fila);
    void setFilaSize(int fila_size);
    void setLongitude(float longitude);
    void setProdutos(Map<String,IProduto> prods);
    void updateFila();
    ILoja clone();
    double distancia(IUser u);
    double tempoEspera();
    void diminuiFila();
}
