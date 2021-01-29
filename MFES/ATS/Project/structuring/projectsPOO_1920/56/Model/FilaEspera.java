package Model;

import Model.Encomendas.Encomenda;
import Model.Encomendas.IEncomenda;

import java.io.Serializable;
import java.time.LocalTime;
import java.util.*;

/**
 * Fila que recebe as encomendas ainda não completas, e estão na fila de espera para a loja correspondente.
 * KEY: String código da loja
 * VALUE: TreeSet com as encomendas para a loja
 */

public class FilaEspera implements IFila, Serializable {
    private Map<String, Set<IEncomenda>> fila;

    public FilaEspera(){
        this.fila = new HashMap<>();
    }

    public void addEncomenda(IEncomenda encomenda){
        String lojaId = encomenda.getLojaID();
        if(this.fila.containsKey(lojaId)){
            this.fila.get(lojaId).add(encomenda);
        }
        else {
            Set<IEncomenda> value = new TreeSet<>(new CompareEncomenda());
            value.add(encomenda);
            this.fila.put(lojaId, value);
        }
    }

    public int removeEncomenda(IEncomenda encomenda){
        String lojaId = encomenda.getLojaID();
        if(this.fila.containsKey(lojaId)){
            this.fila.get(lojaId).remove(encomenda);
        }
        else{
            return -1;
        }
        return 0;
    }

    public Set<IEncomenda> getEncomendas(String codId){
        if(this.fila.containsKey(codId)) return this.fila.get(codId);
        else return null;
    }

    public boolean containsEncTipo (String encId, String codId){
        Set<IEncomenda> value = this.fila.get(codId);
        for(IEncomenda enco : value){
            if(enco.getEncomendaID().equals(encId)) return true;
        }
        return false;
    }

    public IEncomenda getEncomendaTipo (String encId, String codId){
        IEncomenda res = new Encomenda();
        Set<IEncomenda> value = this.fila.get(codId);
        for(IEncomenda enco : value){
            if(enco.getEncomendaID().equals(encId)) return enco;
        }
        return res;
    }

    public IEncomenda existsEncomenda(String id){
        Set<String> set = this.fila.keySet();
        for(String aux : set){
            Set<IEncomenda> value = this.fila.get(aux);
            for(IEncomenda encomenda : value){
                if(encomenda.getEncomendaID().equals(id)){
                    return encomenda;
                }
            }
        }
        return null;
    }

    public IEncomenda getEncomendaRecente(String user){
        return null;
    }
    public boolean existsKey(String codId) {
        return false;
    }

    public String toString() {
        return "Fila: " + fila;
    }

}
