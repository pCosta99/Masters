public interface Linha_EncomendaI extends java.io.Serializable{
    String getCodProduto();
    void setCodProduto(String codProduto);
    String getDesc();
    void setDesc(String desc);
    double getQnt();
    void setQnt(double qnt);
    double getValor();
    void setValor(double valor);
    Linha_Encomenda clone();
    String toString();
    boolean equals(Object o);
}
