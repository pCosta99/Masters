package Model;

import Model.Encomendas.IEncomenda;

import java.util.Set;

public interface IFila {

    void addEncomenda(IEncomenda encomenda);
    int removeEncomenda(IEncomenda encomenda);
    Set<IEncomenda> getEncomendas(String codId);
    boolean containsEncTipo (String encId, String codId);
    IEncomenda getEncomendaTipo (String encId, String codId);
    IEncomenda existsEncomenda(String id);
    boolean existsKey(String codId);
    IEncomenda getEncomendaRecente(String user);
}
