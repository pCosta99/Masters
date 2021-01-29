import java.lang.String;
import java.util.Map;
import java.util.HashMap;   
import java.util.stream.Collectors;

public abstract class Encomenda{
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja; 
    private Map<String,LinhaEncomenda> produtos;
    private float peso;
    
    public Encomenda(){
        this.codEncomenda="n/a";
        this.codUtilizador="n/a";
        this.codLoja="n/a";
        this.produtos=new HashMap<>();
        this.peso=0;
    }
    
    public Encomenda(String codE, String codU, String codL,float peso, Map<String,LinhaEncomenda> prod){
        this.codEncomenda=codE;
        this.codUtilizador=codU;
        this.codLoja=codL;
        this.produtos=prod;
        this.peso=peso;
    }
    
    public Encomenda (Encomenda e){
        this.codEncomenda=e.getCodEncomenda();
        this.codUtilizador=e.getCodUtilizador();
        this.codLoja=e.getCodLoja();
        this.produtos=e.getProdutos();
        this.peso=e.getPeso();
    }

    public String getCodEncomenda(){
        return this.codEncomenda;
    }

    public void setCodEncomenda(String cod){
        this.codEncomenda=cod;
    }

    public String getCodUtilizador(){
        return this.codUtilizador;
    }

    public void setCodUtilizador(String cod){
        this.codUtilizador=cod;
    }
    
    public String getCodLoja(){
        return this.codLoja;
    }

    public void setCodLoja(String cod){
        this.codLoja=cod;
    }
    
    public float getPeso(){
        return this.peso;
    }
    
    public void setPeso(float p){
        this.peso=p;
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("CodEncomenda:").append(this.codEncomenda).append(",").
             append("CodUtilizador:").append(this.codUtilizador).append(",").
             append("CodLoja:").append(this.codLoja).append(",").
             append(this.produtos).append(",");
        return sb.toString();
    }

    public Map<String,LinhaEncomenda> getProdutos(){
        return this.produtos.values().stream().collect(Collectors.toMap(LinhaEncomenda::getCodProduto,LinhaEncomenda::clone));
    }

    public void setProdutos(Map<String,LinhaEncomenda> prod){
        this.produtos=new HashMap<>();
        prod.entrySet().forEach(e -> {this.produtos.put(e.getKey(),e.getValue().clone());});
    }
    
    public abstract Encomenda clone();
    
    public boolean equals (Object o){
        if (o==this) return true;
        if (o==null || o.getClass()!=this.getClass()) return false;
        Encomenda l=(Encomenda) o;
        return this.codLoja.equals(l.getCodLoja()) &&
            this.codUtilizador.equals(l.getCodUtilizador()) &&
            this.codEncomenda.equals(l.getCodEncomenda()) && 
            this.peso==l.getPeso() &&
            this.produtos.equals(l.getProdutos());
    }
       
    public void addLEncomenda (LinhaEncomenda l){
        this.produtos.put(l.getCodProduto(),l.clone());
        this.peso+=l.getPesoTotal();
    }
    
    public void removeLEncomenda (String cod){
        this.produtos.remove(cod);
    }
    
    public void removeLEncomenda (LinhaEncomenda le){
        this.produtos.remove(le.getCodProduto());
    }
    
    public String getEstadoEncomenda(){
        StringBuilder sb= new StringBuilder();
        this.produtos.values().stream().forEach(v->sb.append(",").append(v.getCodProduto()).append(",").append(v.getDescricao()).append(",")
                                                                            .append(v.getQuantidade()).append(",").append(v.getValorUnitario()));
        return sb.toString();                                                                    
    }
    
    public String getNovaEncomenda(){
        StringBuilder sb=new StringBuilder();
        sb.append("Codigo do utilizador: ").append(this.codUtilizador).append(";").append(this.produtos.toString()).append(".\n");
        return sb.toString();
    }
}



















