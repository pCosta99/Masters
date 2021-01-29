import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class Cliente extends User implements Serializable{

    private Map<String,Encomenda> por_aceitar;
    private List<Encomenda> hist_encomendas;



    /**
     *  Construtores, Getters e Setters
     */

    private Cliente(){
    }

    public Cliente(String username, String nome, Coordenadas x){
        super(username,nome,x);
        this.por_aceitar = new HashMap<>();
        this.hist_encomendas = new ArrayList<>();
    }

    public Cliente(String nome,String username, String pass, Coordenadas x, List<Encomenda> aux,Map<String,Encomenda> aux2){
        super(username,nome,pass,x);
        setHistorico(aux);
        setEncomendas_poraceitar(aux2);
    }

    public Cliente(Cliente c){
        super(c);
        setHistorico(c.getHistorico());
        setEncomendas_poraceitar(c.getEncomendas_poraceitar());
    }

    public Map<String,Encomenda> getEncomendas_poraceitar(){
        Map<String,Encomenda> ret = new HashMap<>();
        for(Map.Entry<String,Encomenda> e : this.por_aceitar.entrySet()){
            ret.put(e.getKey(),e.getValue().clone());
        }
        return ret;
    }

    public void setEncomendas_poraceitar(Map<String,Encomenda> aux) {
        this.por_aceitar = new HashMap<>();
        for(Map.Entry<String,Encomenda> e : aux.entrySet()){
            this.por_aceitar.put(e.getKey(),e.getValue().clone());
        }
    }

    public List<Encomenda> getHistorico(){
        return this.hist_encomendas.stream().map(Encomenda :: clone).collect(Collectors.toList());
    }

    public void setHistorico(List<Encomenda> aux){
        this.hist_encomendas = aux.stream().map(Encomenda :: clone).collect(Collectors.toList());
    }

    /**
     * Adiciona uma encomenda ao histórico.
     * @param enc
     */
    public void addEnc(Encomenda enc){
        if (!this.hist_encomendas.contains(enc)) {
            this.hist_encomendas.add(enc.clone());
        }
    }

    /**
     * Remove uma encomenda das encomendas por aceitar
     * @param enc
     */

    public void removeEnc(Encomenda enc){
        this.por_aceitar.values().remove(enc);
    }

    /**
     * Adicionar uma encomenda às encomendas por aceitar
     * @param userEmpresa
     * @param e
     */

    public void addEnc_poraceitar(String userEmpresa, Encomenda e){
        this.por_aceitar.put(userEmpresa,e.clone());
    }

    /**
     * Obter encomenda a partir do seu código
     * @param cod
     * @return
     */

    public Encomenda getEncomenda(String cod){
        Encomenda e = new Encomenda();
        if(this.por_aceitar.containsKey(cod)){
            e = this.por_aceitar.get(cod).clone();
        }
        return e;
    }

    public Cliente clone(){
        return new Cliente(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
                .append("Histórico:\n").append("\t"+this.hist_encomendas)
                .append("Por aceitar:\n").append("\t"+this.por_aceitar);
            return sb.toString();
    }

    public boolean equals(Object o){
        if (o==this)return true;
        if(o==null || o.getClass()!=this.getClass())return false;
        Loja l = (Loja) o;
        return l.getUsername().equals(((Loja) o).getUsername());
    }

    public int hashCode(){
        int hash = 5;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.hist_encomendas.stream().mapToInt(Encomenda :: hashCode).sum();
        return hash;
    }
}
