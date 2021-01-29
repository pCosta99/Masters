import ClassesAux.GPS;
import java.io.Serializable;
import java.util.Map;
import java.util.HashMap;
public class Loja implements Serializable
{
    private String codLoja;
    private String nome;
    private GPS gps;
    private int fila;
    private Map<String,LinhaEncomenda> catalogo;
    public static double tempoEsperaFila=0.08;//0.08 horas=5 mins
    //private String Menu com os produtos possiveis seu peso por unidade,preco,idProduto, e codDeLoja

     public Loja()
    {
        this.codLoja="n/a";
        this.nome="n/a";
        this.gps=new GPS();
        this.fila=0;
        this.catalogo=new HashMap<>();
    }
    public Loja(String codLoja,String nome,GPS gps)
    {
        this.codLoja=codLoja;
        this.nome=nome;
        this.gps=gps.clone();
        this.fila=0;
        this.catalogo=new HashMap<>();
    }
    public Loja(String codLoja,String nome,double x,double y){
        this.codLoja=codLoja;
        this.nome=nome;
        this.gps=new GPS (x,y);
        this.fila=0;
        this.catalogo=new HashMap<>();
    }
    public Loja(Loja loja){
        this.codLoja=loja.getCodLoja();
        this.nome=loja.getNome();
        this.gps=loja.getGps();
        this.fila=loja.getFila();
        this.catalogo=loja.getCatalogo();
    }
    //L29,TV e Comunicaçoes,-97.98,80.00
    public Loja(String loja)throws NullPointerException,NumberFormatException{
     var array = loja.split(",");
     this.codLoja=array[0];
     this.nome=array[1];
     this.gps=new GPS(Double.parseDouble(array[2]),Double.parseDouble(array[3]));
     this.fila=0;
     this.catalogo=new HashMap<>();
    }
    public String getCodLoja(){return this.codLoja;}
    public String getNome(){return this.nome;}
    public int getFila(){return this.fila;}
    public static double getTempoEsperaFila(){return tempoEsperaFila;}
    public static void setTempoEsperaFila(double tEspera){Loja.tempoEsperaFila=tEspera;}
    public GPS getGps(){return this.gps.clone();}
    public Map<String,LinhaEncomenda> getCatalogo(){
        Map<String,LinhaEncomenda> res= new HashMap<>();
        for(LinhaEncomenda a:this.catalogo.values()){
            res.put(a.getCodProduto(),a.clone());
        }
        return res;
    }
    public  void setCatalogo(Map<String,LinhaEncomenda> cat){
        this.catalogo= new HashMap<>();
        for(LinhaEncomenda a:cat.values()){
            this.catalogo.put(a.getCodProduto(),a.clone());
        }
    }
    public void setCodLoja(String loja){this.codLoja=loja;}
    public void setNome(String nome){this.nome=nome;}
    public void setGps(GPS gps){this.gps=gps.clone();}
    public void setFila(int fila){this.fila=fila;}
    public Loja clone (){return new Loja(this);}
    public boolean equals(Object o){
    if (this==o) return true;
    if ((o == null) || (this.getClass() != o.getClass()))
    return false;
    Loja p = (Loja) o;
    return (p.getCodLoja().equals(this.codLoja) && 
    p.getNome().equals(this.nome) &&
    p.getGps().equals(this.gps)
     );
    }
    public String toString(){
    StringBuilder sb =new StringBuilder();
    sb.append("Loja:").append(this.codLoja);
    sb.append(",").append(this.nome);
    sb.append(this.gps);
    sb.append(this.fila).append("\n");
    return sb.toString();
    }
    public void adicionaProduto(String codProd,String descricao,double preco) {
        this.catalogo.putIfAbsent(codProd,new LinhaEncomenda(codProd,descricao,0,preco));
    }
    public boolean tenhoProduto(String codProd){return this.catalogo.containsKey(codProd);}
    public String showCatalogo (){
        StringBuilder sb=new StringBuilder();
        for(LinhaEncomenda a:this.catalogo.values()){
            sb.append("Produto:").append(a.getCodProduto()).append(",")
            .append(a.getDescricao()).append(",").append(a.getPreco()).append("\n");
        }
        return sb.toString();
    }
}
