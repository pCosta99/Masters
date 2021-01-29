package View;

import Model.Catalogos.ICatalogoProds;
import Model.Encomendas.Entrega;
import Model.Encomendas.IEncomenda;
import Model.Encomendas.IEntrega;
import Model.Tipos.ITipo;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public interface IAppView {
    void preInicio();
    void inicio ();
    void registo ();
    void printMensagem(String s);
    void login(int x);
    void reg (int x);
    void Voluntariomode();
    void userMode();
    void modeLoja();
    void modeEmpresa();
    void encomendasEspera(Set<IEncomenda> res);
    void encomendasFila(Set<IEntrega> res);
    void listagem(IEntrega[] res);
    void prepara(int x);
    void transportes(char opcao, HashSet<ITipo> res);
    void classificacao();
    void classificacoes(HashMap<String,Integer> res);
    void infos();
    void tipodeEncomenda(int x);
    void top (ITipo[] res, Integer[] n);
    void top2 (String[] res, Float[] dist);
    void lista (List<IEntrega> res);
    void vendasLoja(Set<IEncomenda> res);

}
