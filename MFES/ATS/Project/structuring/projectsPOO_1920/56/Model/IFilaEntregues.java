package Model;

import Model.Encomendas.IEntrega;

import java.time.LocalDate;
import java.util.HashMap;
import java.util.Set;

public interface IFilaEntregues {
    void addEncomenda(IEntrega e);
    int removeEncomenda(IEntrega e);
    Set<IEntrega> getEntregas(String codId);
    Set<IEntrega> getEntregasFalse(String codId);
    Set<IEntrega> getEntregasTrue(String codId);
    Set<IEntrega> getMedicamentos(String codId);
    IEntrega getEntrega(String id);
    boolean containsEncFalse (String encId);
    HashMap<String,Integer> getClassificacoes(String keyTransp);
    int getFaturacao(String transp, LocalDate date);
    String[] top10Empresas();
    Float[] distEmpresa(String[] res);

}
