package interfaces;

public interface IProduto {
    String getCode();
    double getQtd();
    String getProduto();
    double getValor();
    double getPeso();
    boolean getMedicine();
    IProduto clone();
    void update(double q, double valorUnid, double pesoUnid);
}
