
import java.util.stream.Collectors;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import static java.lang.System.out;
import java.io.Serializable;
public class Utilizador implements Serializable
{
    
    private String cod;  //codigo do Utilizador
    
    private String nome;
    
    private Map<String, ArrayList<Pair>> historico;  //String (Cod Transportador) -> Pair (cod Encomenda, preco)
    
    private GPS local;
    
    private int quantas = 0;
    
    
    public Utilizador(String cod, String nome, GPS local)
    {
        this.historico = new HashMap();
        this.cod = cod;
        this.nome = nome;
        this.local = local;        
        
    }
     
    public String getNome(){
        return this.nome;
    }
    public GPS getLocal(){
        return this.local;
    }
    
    public String getCod(){
        return this.cod;
    }
    
    public Map getHistorico (){
        return new HashMap (this.historico);
    }
    
    public String getHistoricoFilteredString (String codTransp){
        String res = "";
        for(Pair p: this.historico.get(codTransp)){
            res += "Encomenda: " + p.p1() + " Preco: " + p.p2() +"\n";
        }
        return res;
    }
    
    public String getHistoricoString (){
        String res = "";
        for(ArrayList<Pair> a:  this.historico.values().stream().collect(Collectors.toSet())){
            for (Pair p: a){
                res += "Encomenda: " + p.p1() + " Preco: " + p.p2()+"\n";
            }
        }
        return res;
    }
    
    public void addHistorico(String codTransp, String codEnc, String preco){
        if (this.historico.keySet().contains(codTransp)){
            this.historico.get(codTransp).add(new Pair(codEnc, preco));
        }
        else{
            ArrayList<Pair> n = new ArrayList<Pair> ();
            n.add(new Pair(codEnc,preco));
            this.historico.put(codTransp, n);
        }
    }
    
    public void addHistorico(String codTransp, String codEnc, double preco){
        if (this.historico.keySet().contains(codTransp)){
            this.historico.get(codTransp).add(new Pair(codEnc, "" + preco));
        }
        else{
            ArrayList<Pair> n = new ArrayList<Pair> ();
            n.add(new Pair(codEnc,""+preco));
            this.historico.put(codTransp, n);
        }
    }
    
    public boolean equals(Object o){
        if (o== null){
            return false;
        }
        else if(o.getClass() != Utilizador.class){
            return false;
        }
        else{
            Utilizador that = (Utilizador) o;
            return (this.cod.equals(that.getCod()));
        }
    }
    
    public void addNEnc(){
        this.quantas += 1;
    }
    
    public int getQuantas(){
        return this.quantas;
    }

}
