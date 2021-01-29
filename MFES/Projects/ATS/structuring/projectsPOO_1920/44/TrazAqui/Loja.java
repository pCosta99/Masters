import java.time.LocalTime;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.Scanner;
import java.time.LocalDate;


public class Loja extends Account
{
    private String nomeLoja;
    private String codLoja;
    private boolean sinal;
    private Location loc;
    private Map<String,Encomenda> encomendas;
    
    public Loja(){
        super();
        this.loc = new Location();
        this.codLoja = "";
        this.sinal = false;
        this.encomendas = new HashMap<>();
    }
    
    public Loja(String em,String pa,List<Encomenda> re,
                String n,String c,Location loc){
        super(n,em,pa,re, c);
        this.loc = loc;
        this.nomeLoja = n;
        this.codLoja = c;
        this.sinal = false;
        this.encomendas = new HashMap<>();
    }
    
    public Loja(Loja l){
        super(l);
        this.loc = l.getLoc();
        this.codLoja = l.getCodLoja();
        this.sinal = l.getSinal();
        this.encomendas = l.getEncomendas();
    }
    
    public Location getLoc(){
        return this.loc;
    }
    
    public void setLoc(Location l){
        this.loc = l;
    }
    
    public String getCodLoja(){return this.codLoja;}
    
    public boolean getSinal(){return this.sinal;}
            
    public Encomenda getEncomenda(String cod){
        return this.encomendas.get(cod).clone();
    }
    
    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> res = new HashMap<>();
        
        for(Map.Entry<String,Encomenda> entry : this.encomendas.entrySet()){
            res.put(entry.getKey(),entry.getValue().clone());
        }
        return res;
    }
    
    public void setEncomendas(Map<String, Encomenda> enc){
        this.encomendas = new HashMap<>();
        enc.values().forEach(a -> this.encomendas.put(a.getCliente(),a.clone()));
    }
    
    public boolean setPronto(Encomenda e){
        if(this.encomendas.containsValue(e))
            return true;
        else
            return false;
    }
    
    public void insereEncomenda(Encomenda e){
        this.encomendas.put(e.getCliente(),e.clone());
    }
    
    public void removeEncomenda(String nome){
        this.encomendas.remove(nome);
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Loc:").append(this.loc + "\n")
        .append("Codigo: ").append(this.codLoja).append("\n");                            
        return super.toString() + sb.toString();                    
    }
    
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        Loja l = (Loja) o;
        return this.codLoja == l.getCodLoja();
    }

    public void aceiEncomenda(Map<String,Boolean> aceites)
    {
        Scanner input = new Scanner(System.in);
        List<Encomenda> paraAceitar = new ArrayList<>();
        List<Encomenda> lista = super.getEncomenda();

        for(Encomenda enco : lista)
        {
            if(aceites.get(enco.getReferencia()).equals(false))
            {
                enco.setDatabusca(LocalDate.now());
                paraAceitar.add(enco);
            }
        }

        System.out.println("Escolha a encomenda a aceitar:");
        for(int i = 0; i < paraAceitar.size() ; i++)
            System.out.println((i+1) + "-" + paraAceitar.get(i).getReferencia());
        int option = input.nextInt();

        aceites.put(paraAceitar.get(option-1).getReferencia(),true);

    }
    
    public Loja clone(){
        return new Loja(this);
    }
}
