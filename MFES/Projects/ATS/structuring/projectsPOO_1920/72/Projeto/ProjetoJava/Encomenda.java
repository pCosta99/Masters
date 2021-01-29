import java.time.LocalDate;
import java.util.ArrayList;
import java.io.Serializable;
public abstract class Encomenda implements Serializable
{
  private String codEncomenda;
  private String codUtilizador;
  private String codLoja;
  private double peso;
  private ArrayList<LinhaEncomenda> lista;
  
    public Encomenda()
    {
     this.codEncomenda="n/a";
     this.codUtilizador="n/a";
     this.codLoja="n/a";
     this.peso=0;
     this.lista=new ArrayList<>();
    }
    public Encomenda(String codEncomenda,String codUtilizador,String codLoja,double peso,ArrayList<LinhaEncomenda> l)
    {
     this.codEncomenda=codEncomenda;
     this.codUtilizador=codUtilizador;
     this.codLoja=codLoja;
     this.peso=peso;
     this.setLista (l);
    }
    public Encomenda (Encomenda a)
    {
     this.codEncomenda=a.getCodEncomenda();
     this.codUtilizador=a.getCodUtilizador();
     this.lista=a.getLista();
     this.codLoja=a.getCodLoja();
     this.peso=a.getPeso();
     this.lista=a.getLista();
    }
    //e6813,u81,l8,77.811,p34,Farinha,
    public Encomenda (String enc) throws NullPointerException,NumberFormatException{
     ArrayList<LinhaEncomenda> le = new ArrayList<>();
     var array = enc.split(",");
     this.codEncomenda=array[0];
     this.codUtilizador=array[1];
     this.codLoja=array[2];
     this.peso=Double.parseDouble(array[3]);
     for(int i=4;i<array.length;i=i+4){
        //codProduto,Descricao,quant,preco
        LinhaEncomenda l =new LinhaEncomenda(array[i],array[i+1],Double.parseDouble(array[i+2]),Double.parseDouble(array[i+3])); 
        le.add(l);
        }
     this.lista=le;
    }
    public ArrayList<LinhaEncomenda> getLista ()
    {
       ArrayList<LinhaEncomenda> le =new ArrayList<>(); 
       for(LinhaEncomenda e:this.lista)
        le.add(e.clone());
       return le;
    }
    public void setLista(ArrayList<LinhaEncomenda> l)
    {
        this.lista=new ArrayList<> ();
        for(LinhaEncomenda e:l)
        this.lista.add(e.clone());
    }
    public String getCodEncomenda(){
        return this.codEncomenda;
    }
    public void setCodEncomenda(String codEncomenda){
    this.codEncomenda=codEncomenda;
    }
    public String getCodUtilizador(){
        return this.codUtilizador;
    }
    public void setCodUtilizador(String codUtilizador){
    this.codUtilizador=codUtilizador;
    }
    public String getCodLoja(){
        return this.codLoja;
    }
    public void setCodLoja(String codLoja){
    this.codLoja=codLoja;
    }
    public double getPeso(){
        return this.peso;
    }
    public void setPeso(int peso){
    this.peso=peso;
    }
    
    public abstract Encomenda clone();
    public abstract String toString();
    
    public double calculaValorTotal (){
    double res=0;
        for(LinhaEncomenda e:this.lista)
        res+=e.calculaValorLinhaEncomenda();
    return res;
    }
    public int numeroTotalProdutos ()
    {
    int res=0;
        for(LinhaEncomenda e:this.lista)
        res+=e.getQuantidade();
    return res;
    }
    public boolean existeProdutoEncomenda(String codProduto)
    {
    boolean res=false;
        for(LinhaEncomenda e:this.lista)
        if (e.getCodProduto().equals(codProduto)) res=true;
    return res;
    }
    public void adicionaLinha(LinhaEncomenda linha)
    {
     this.lista.add(linha);
    }
    public void removeProduto(String codProd)
    {
    this.lista.removeIf(a->a.getCodProduto().equals(codProd));
    }
    public boolean equals(Object e)
    {
     if (e==this) return true;
     if (e==null||e.getClass()!=this.getClass()) return false;
     Encomenda enc = (Encomenda) e;
     return enc.getLista().equals(this.lista) &&
     enc.getCodEncomenda().equals(this.codEncomenda) &&
     enc.getCodUtilizador().equals(this.codUtilizador) &&
     enc.getCodLoja().equals(this.codLoja);
    }
} 
