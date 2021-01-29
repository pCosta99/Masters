import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Classe que guarda todas as encomendas num HashMap, em que a chave é o
 * username do utilizador que pede a ecomenda,
 * e o seu conteudo é a Lista de Encomendas
 */
public class EncomendasDB {
    private HashMap<String, List<Encomenda>> encomendasDB;

    public EncomendasDB(){
        this.encomendasDB = new HashMap<>();
    }

    public EncomendasDB(HashMap<String, List<Encomenda>> encomendasDB){
        setEncomendasDB(encomendasDB);
    }

    public EncomendasDB(EncomendasDB e){
        setEncomendasDB(e.getEncomendasDB());
    }

    public HashMap<String, List<Encomenda>> getEncomendasDB() {
        HashMap<String,List<Encomenda>> copia = new HashMap<>();
        for(Map.Entry<String,List<Encomenda>> edb: this.encomendasDB.entrySet()){
            copia.put(edb.getKey(),(edb.getValue()).stream().map(Encomenda::clone)
                                                    .collect(Collectors.toList()));
        }
        return copia;
    }

    public void setEncomendasDB(HashMap<String,List<Encomenda>> edb){
        this.encomendasDB = new HashMap<>();
        edb.entrySet().forEach(e -> this.encomendasDB.put(e.getKey(),(e.getValue()).stream().map(Encomenda::clone)
                .collect(Collectors.toList())));
    }

    public void addUser(String user){
        List<Encomenda> l = new ArrayList<>();
        this.encomendasDB.put(user,l);
    }

    public void removeUser(String user){
        this.encomendasDB.remove(user);
    }

    public void addEncomenda(Encomenda e){
        if(encomendasDB.containsKey(e.getUtilizador())){
            this.encomendasDB.get(e.getUtilizador()).add(e.clone());
        }
        else{
            List<Encomenda> l = new ArrayList<>();
            l.add(e.clone());
            this.encomendasDB.put(e.getUtilizador(),l);
        }
    }

    public void removeEncomenda(Encomenda e){
        this.encomendasDB.get(e.getUtilizador()).removeIf(e::equals);
    }

    public EncomendasDB clone(){
        return new EncomendasDB(this);
    }

    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        EncomendasDB edb = (EncomendasDB) o;
        return this.getEncomendasDB().equals(edb.getEncomendasDB());
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.encomendasDB + "\n");
        return sb.toString();
    }

}
