package Projeto.Interfaces;
import Projeto.Exceptions.IdRepetidoException;

import java.io.Serializable;
import java.util.Collection;

public interface ILoja extends Serializable, IEntidade {
    void setSize(int n);
    void setDFila(boolean dados);
    Collection<IProduto> getListProds();
    boolean getDFila();
    void setTempMed(float tempo);
    void setListProds(Collection<IProduto> listProds);
    void addProduto(IProduto p) throws IdRepetidoException;
    void removeProduto(String codigo);
    float getTempMed();
    ILoja clone();
}
