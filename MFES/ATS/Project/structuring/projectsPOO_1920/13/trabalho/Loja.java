import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.List;
import java.util.Collections;
import java.util.*;
public class Loja
{
    private String codLoja;
    private String nome;
    private double latitude,longitude;
    private Map<String, LinhaEncomenda > produtos;
    /**
     * COnstrutor para objetos da classe user
     */
    public Loja()
    {
        this.codLoja= new String();
        this.nome= new String();
        this.latitude=0;
        this.longitude=0;
        this.produtos=new HashMap<>();
    }
    public Loja(String codLoja,String name,double lat,double longi)
    {
        this.codLoja= codLoja;
        this.nome= name;
        this.latitude=lat;
        this.longitude=longi;
        this.produtos=new HashMap<>();
    }
     public Loja(Loja lo)
    {
        this.codLoja= lo.getcod();
        this.nome= lo.getnome();
        this.latitude=lo.getlat();
        this.longitude=lo.getlong();
        this.produtos=lo.getAllprodutos();
    }
    public Map<String,LinhaEncomenda> getAllprodutos() {
        return this.produtos.entrySet().stream().collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue().clone()));
    }
    public void setLinhas(Map<String,LinhaEncomenda> enc) {
      this.produtos = new HashMap<>();
      enc.values().forEach(e -> this.produtos.put(e.getcod(),e.clone()));

    }
    public String getcod(){
        return this.codLoja;
    }
    public String getnome(){
        return this.nome;
    }
    public double getlat(){
        return this.latitude;
    }
    public double getlong(){
        return this.longitude;
    }
    public Loja clone(){
        return new Loja(this);
    }
    public void adicionaproduto(LinhaEncomenda l){
       this.produtos.put(l.getcod(),l.clone());
   }
}
