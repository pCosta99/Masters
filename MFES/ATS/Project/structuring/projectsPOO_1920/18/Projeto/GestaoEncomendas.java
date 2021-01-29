import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.time.LocalDateTime;
import java.util.List;
import java.util.ArrayList;

public class GestaoEncomendas{
    
    private Map<String,Encomenda> encomendas;
    
    public GestaoEncomendas(){
        encomendas=new HashMap<>();
    }
    
    public GestaoEncomendas(Map<String,Encomenda> e){
        this.setEncomendas(e);
    }
    
    public GestaoEncomendas(GestaoEncomendas ge){
        this.encomendas=ge.getEncomendas();
    }
    
    public Map<String,Encomenda> getEncomendas(){
        return this.encomendas.entrySet().stream().collect(Collectors.toMap(e->e.getKey(),e->e.getValue().clone()));
    }
    
    public void setEncomendas(Map<String,Encomenda> enc){
        this.encomendas=new HashMap<>();
        enc.entrySet().forEach(e -> {this.encomendas.put(e.getKey(),e.getValue().clone());});
    }
    
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GestaoEncomendas ge = (GestaoEncomendas) obj;
        return ge.getEncomendas().equals(this.encomendas);
    }
    
    public String toString(){
        return this.encomendas.toString();
    }
    
    public GestaoEncomendas clone(){
        return new GestaoEncomendas(this);
    }
    
    public Encomenda getEncomenda (String cod){
        return this.encomendas.get(cod).clone();
    }
    
    public void addEncomenda (Encomenda enc){
        this.encomendas.put(enc.getCodEncomenda(),enc.clone());
    }
    
    public void removeEncomenda (String cod){
        this.encomendas.remove(cod);
    }
    
    public void removeEncomenda (Encomenda enc){
        this.encomendas.remove(enc.getCodEncomenda());
    }
    
    public void addEncomendaEfetuada(Encomenda enc, LocalDateTime l,String codT,float custo){
        EncomendaEfetuada e = new EncomendaEfetuada(enc,l,codT,custo);
        this.encomendas.put(e.getCodEncomenda(),e);
    }
    
    public List<String> encomendas(){
        List<String> enc=new ArrayList<>();
        return this.encomendas.keySet().stream().collect(Collectors.toList());
    }    

    public int total(){
        return this.encomendas.size();
    }
    
    public String getEstadoEncomendas(){
        StringBuilder sb = new StringBuilder();
        this.encomendas.values().stream().forEach(e->sb.append("Encomenda:").append(e.getCodEncomenda()).append(",")
                                          .append(e.getCodUtilizador()).append(",").append(e.getCodLoja()).append(",")
                                          .append(e.getPeso()).append(e.getEstadoEncomenda()).append("\n"));
        return sb.toString();
    }
    
    //(String codE, String codU, String codL,codT,float peso,float custo, Map<String,LinhaEncomenda> prod,LocalDateTime date)
    public String getStrEstadoEncEfe(){
        StringBuilder sb = new StringBuilder();
        this.encomendas.values().stream().filter(e -> e instanceof EncomendaEfetuada).map(e -> (EncomendaEfetuada) e).
                                          forEach(e->sb.append("Encomenda efetuada:").append(e.getCodEncomenda()).append(",")
                                          .append(e.getCodUtilizador()).append(",").append(e.getCodLoja()).append(",")
                                          .append(e.getCodTransportador()).append(",").append(e.getCusto()).append(",")
                                          .append(e.getPeso()).append(",").append(Parse.timeStr(e.getTempo()))
                                          .append(e.getEstadoEncomenda()).append("\n"));
        return sb.toString();
    }
    
    public boolean temEncomenda(String cod){
        return this.encomendas.containsKey(cod);
    }
    
    public List<String> listaEncomendas(){
        return this.encomendas.keySet().stream().collect(Collectors.toList());
    }
    
    public boolean contains (String cod){
        return this.encomendas.containsKey(cod);
    }
    
    public String getCodUtilizador(String codEnc){
        return this.encomendas.get(codEnc).getCodUtilizador();
    }
}


































