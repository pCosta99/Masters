package Model;

import Model.Encomendas.IEncomenda;
import Model.Encomendas.IEntrega;
import Model.Tipos.Empresa;
import Model.Tipos.ITipo;
import Model.Tipos.User;
import Model.Tipos.Voluntario;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.*;

/**
 * KEY : transporte
 */
public class FilaEntregues implements IFilaEntregues, Serializable {
    private Map<String, Set<IEntrega>> fila;

    public FilaEntregues(){
        this.fila = new HashMap<>();
    }


    public void addEncomenda(IEntrega e) {
        String transpID = e.getTransporte().getId();
        if(this.fila.containsKey(transpID)){
            this.fila.get(transpID).add(e);
        }
        else {
            Set<IEntrega> value = new TreeSet<>(new CompareEntrega());
            value.add(e);
            this.fila.put(transpID, value);
        }
    }

    public int removeEncomenda(IEntrega e) {
        String transpID = e.getTransporte().getId();
        if(this.fila.containsKey(transpID)){
            this.fila.get(transpID).remove(e);
            return 0;
        }
        return -1;
    }


    public Set<IEntrega> getEntregas(String codId) {
        if(this.fila.containsKey(codId)) return this.fila.get(codId);
        else return null;
    }

    public Set<IEntrega> getEntregasFalse(String codId){
        Set<IEntrega> res = new HashSet<>();
        if(this.fila.containsKey(codId)){
            Set<IEntrega> value = this.fila.get(codId);
            for(IEntrega e : value){
                if(!e.getEntregue()) res.add(e);
            }
        }
        return res;
    }

    public Set<IEntrega> getEntregasTrue(String codId){
        Set<IEntrega> res = new HashSet<>();
        if(this.fila.containsKey(codId)){
            Set<IEntrega> value = this.fila.get(codId);
            for(IEntrega e : value){
                if(e.getEntregue()) res.add(e);
            }
        }
        return res;
    }

    public Set<IEntrega> getMedicamentos(String codId){
        Set<IEntrega> set = new HashSet<>();
        if(this.fila.containsKey(codId)) {
            Set<IEntrega> value = this.fila.get(codId);
            for(IEntrega e : value){
                if(e.getEncomenda().getMedicamentos()) set.add(e);
            }
        }
        return set;
    }

    public boolean containsEncFalse (String encId) {
        Set<String> set = this.fila.keySet();
        for(String aux : set){
            Set<IEntrega> value = this.fila.get(aux);
            for(IEntrega encomenda : value){
                if(encomenda.getEncomenda().getEncomendaID().equals(encId)){
                    return true;
                }
            }
        }
        return false;
    }


    public IEntrega getEntrega(String id) {
        Set<String> set = this.fila.keySet();
        for(String aux : set){
            Set<IEntrega> value = this.fila.get(aux);
            for(IEntrega encomenda : value){
                if(encomenda.getEncomenda().getEncomendaID().equals(id)){
                    return encomenda;
                }
            }
        }
        return  null;
    }

    public HashMap<String,Integer> getClassificacoes(String keyTransp){
        HashMap<String,Integer> res = new HashMap<>();
        Set<IEntrega> value = this.fila.get(keyTransp);
        for(IEntrega e : value){
            if(e.getEntregue()){
                if(e.getTransporte() instanceof Voluntario){
                    if((((Voluntario) e.getTransporte()).getVolunteer_rating())!=0){
                        res.put(e.getEncomenda().getUserID(),(((Voluntario) e.getTransporte()).getVolunteer_rating()));
                    }
                }
                else{
                    if((((Empresa) e.getTransporte()).getClassificacao())!=0){
                        res.put(e.getEncomenda().getUserID(),(((Empresa) e.getTransporte()).getClassificacao()));
                    }
                }
            }
        }
        return res;
    }

    public int getFaturacao(String transp, LocalDate date){
        int res = 0;
        Set<IEntrega> value = this.fila.get(transp);
        for(IEntrega e : value){
            if(e.getDataEntrega().equals(date)){
                res += e.getPrecoTotal();
            }
        }
        return res;
    }

    /**
     * Só vai até 10 com o array na view
     * @return
     */
    public String[] top10Empresas(){
        Set<String> set = this.fila.keySet();
        String[] array = set.toArray(new String[set.size()]);
        Arrays.sort(array, new Comparator<String>(){
            public int compare(String c1, String c2){
                float res1 = 0; float res2 = 0;
                for(IEntrega e : getEntregas(c1)){
                    res1 += e.getDistPercorrida();
                }
                for(IEntrega e : getEntregas(c2)){
                    res2 += e.getDistPercorrida();
                }
                if(res1>res2) return -1;
                else if(res1<res2) return 1;
                else return 0;
            }
        });
        return array;
    }

    public Float[] distEmpresa(String[] res){
        Float[] res2 = new Float[res.length];
        float res1=0;
        for(int i=0; i<res.length;i++){
            for(IEntrega e : getEntregas(res[i])){
                res1 += e.getDistPercorrida();
            }
            res2[i] = res1;
        }
        return res2;
    }


    public String toString() {
        return "Fila: " + fila;
    }
}
