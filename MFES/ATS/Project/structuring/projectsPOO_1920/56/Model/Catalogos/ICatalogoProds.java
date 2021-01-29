package Model.Catalogos;

import Model.Encomendas.*;

import java.util.HashMap;
import java.util.HashSet;

public interface ICatalogoProds {
    HashMap<String,IProduto> getCatProds();
    String toString();
    boolean existsProd (IProduto produto);
    void insereProd(IProduto produto);
    int totalProds();
    IProduto getProd (String codProduto);
    boolean existsProdStr(String codProd);
}
