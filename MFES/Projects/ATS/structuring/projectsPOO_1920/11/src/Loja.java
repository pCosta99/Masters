import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;

/**
 * Classe que trata das Loja que produzem as encomendas
 * 
 * @author Rui Cunha
 * @version 06/04/2020
 */
public abstract class Loja extends User
{
    private Map<String,Encomenda> encomendas;//encomendas prontas a ser entregues
    private Map<String,Integer> classificacao;//classificacoes feitas pelos utilizadores

    //Construtor vazio
    public Loja()
    {
        super();
        this.encomendas = new HashMap<>();
        this.classificacao = new HashMap<>();
    }
    
    //Construtor por parametros
    public Loja(String username, String nome,String password,Localizacao local,HashMap<String,Encomenda> encomendas, Map<String,Integer> classificacao)
    {
        super(username,nome,password,local);
        setEncomendas(encomendas);
        setClassificacao(classificacao);
    }
    
    //Construtor copia
    public Loja(Loja novaloja)
    {
        super(novaloja);
        setEncomendas(novaloja.getEncomendas());
        setClassificacao(novaloja.getClassificacao());
    }
    
    //Getters
    public HashMap<String, Encomenda> getEncomendas() {
        HashMap<String,Encomenda> copia = new HashMap<>();
        for(Map.Entry<String,Encomenda> edb: this.encomendas.entrySet()){
            copia.put(edb.getKey(),edb.getValue().clone());
        }
        return copia;
    }

    public HashMap<String, Integer> getClassificacao() {
        HashMap<String,Integer> copia = new HashMap<>();
        for(Map.Entry<String,Integer> edb: this.classificacao.entrySet()){
            copia.put(edb.getKey(),edb.getValue());
        }
        return copia;
    }
    
    //Setters
    public void setEncomendas(HashMap<String,Encomenda> edb){
        this.encomendas = new HashMap<>();
        edb.entrySet().forEach(e -> this.encomendas.put(e.getKey(),(e.getValue()).clone()));
    }

    public void setClassificacao(Map<String, Integer> classificacao) {
        this.classificacao = new HashMap<>();
        classificacao.entrySet().forEach(e -> this.classificacao.put(e.getKey(),e.getValue()));
    }

    public void addEncomenda(Encomenda e){
        this.encomendas.put(e.getCodigo(),e.clone());
    }

    public void removeEncomenda(String s){this.encomendas.remove(s);}

    public void addClassificacao(String name, int classificacao)
    {
        this.classificacao.put(name,classificacao);
    }

    public double classMedia(){
        double sum = 0;
        int i = 0;
        for(Map.Entry<String,Integer> e : this.classificacao.entrySet())
        {
            sum = e.getValue() + sum;
            i += 1;
        }
        return sum/i;
    }

    //Clone
    public abstract Loja clone();
    
    //toString
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
        .append("Encomendas : ").append(this.encomendas.toString() + "\n")
        .append("Classificações : ").append(this.classificacao.toString() + "\n");
        return sb.toString();
    }
    
    //Equals
    public boolean equals(Object o)
    {
        if(this == o)
            return true;
        if((o==null) || (this.getClass() != o.getClass()))
            return false;
            
        Loja p = (Loja) o;
        return(super.equals(p) && this.getEncomendas().equals(p.getEncomendas()))
                && this.getClassificacao().equals(p.getClassificacao());
    }
}
