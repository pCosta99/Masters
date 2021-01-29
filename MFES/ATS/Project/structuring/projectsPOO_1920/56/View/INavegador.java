package View;

import Model.Catalogos.ICatalogoProds;
import Model.Tipos.ILoja;
import Model.Tipos.ITipo;
import Model.Tipos.Loja;

import java.util.HashSet;
import java.util.Set;

public interface INavegador {
    void divide(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao);
    void proxima(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao);
    void anterior(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao);
    void total(ICatalogoProds cat,Set<ITipo> lojas, int opcao);
    void escolha(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao, int num);
    void menu ();
}