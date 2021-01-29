import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.List;
import java.util.Collections;
import java.util.*;
public class encomenda
{   private String codencomenda;
    private String coduser;
    private String codloja;
    private double peso;
    private Map<String, LinhaEncomenda > linhas;
    public encomenda()
    {
        this.codencomenda= new String();
        this.coduser= new String();
        this.codloja=new String();
        this.peso=0;
        this.linhas=new HashMap<>();
    }

    public encomenda(String codenc,String user,String loja,double peso){
        this.codencomenda= codenc;
        this.coduser= user;
        this.codloja=loja;
        this.peso=peso;
        this.linhas=new HashMap<>();
    }
    public encomenda(encomenda e){
        this.codencomenda= e.getcod();
        this.coduser= e.getuser();
        this.codloja=e.getloja();
        this.peso=e.getpeso();
        this.linhas=e.getAllprodutos();
    }
    public Map<String,LinhaEncomenda> getAllprodutos() {
        return this.linhas.entrySet().stream().collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue().clone()));
    }
    public String getcod(){
        return this.codencomenda;
    }
    public double getpeso(){
        return this.peso;
    }
    public String getuser(){
        return this.coduser;
    }
    public String getloja(){
        return this.codloja;
    }
    public encomenda clone(){
        return new encomenda(this);
    }
    public void setLinhas(Map<String,LinhaEncomenda> enc) {
      this.linhas = new HashMap<>();
      enc.values().forEach(e -> this.linhas.put(e.getcod(),e.clone()));

    }
    public void adicionaLinha(LinhaEncomenda l){
       this.linhas.put(l.getcod(),l.clone());
    }
    
}