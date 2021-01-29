package Model.Tipos;


import Model.Catalogos.ICatalogoProds;
import Model.Encomendas.Encomenda;

import java.time.LocalDateTime;
import java.util.ArrayList;

public interface ILoja {

    int getPessoasFila();
    double getTempoPessoa();
    void setPessoasFila(int pessoasFila);
    void setTempoPessoa(double tempoPessoa);
    ICatalogoProds getStock();
    void setStock(ICatalogoProds stock);

    Loja clone();
    boolean equals(Object o);
    int hashCode();
    String toString();
    String getIdSuper (ILoja loja);
}
