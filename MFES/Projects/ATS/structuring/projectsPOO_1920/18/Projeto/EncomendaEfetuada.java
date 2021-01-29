import java.time.LocalDateTime;
import java.util.Map;


public class EncomendaEfetuada extends Encomenda{
    private LocalDateTime tempo;
    private String codTra;
    private float custoEntrega;

    public EncomendaEfetuada(){
        super();
        this.tempo=LocalDateTime.MIN;
        this.codTra=null;
        this.custoEntrega=0;
    }
    
    public EncomendaEfetuada(String codE, String codU, String codL,String codT,float peso,float custo,
            Map<String,LinhaEncomenda> prod,LocalDateTime date){
        super(codE,codU,codL,peso,prod);
        this.setTempo(date);
        this.codTra=codT;
        this.custoEntrega=custo;
    }
    
    public EncomendaEfetuada(EncomendaEfetuada ef){
        super(ef.getCodEncomenda(),ef.getCodUtilizador(),ef.getCodLoja(),ef.getPeso(),ef.getProdutos());
        this.tempo=ef.getTempo();
        this.codTra=ef.getCodTransportador();
        this.custoEntrega=ef.getCusto();
    }
    
    public EncomendaEfetuada (Encomenda enc, LocalDateTime l,String codT,float custo){
        super(enc);
        this.tempo=l;
        this.codTra=codT;
        this.custoEntrega=custo;
    }
    
    public LocalDateTime getTempo(){
        return this.tempo;
    }
    
    public void setTempo(LocalDateTime date){
        this.tempo=date;
    }
    
    public String getCodTransportador(){
        return this.codTra;
    }
    
    public void setCodTransportador(String codT){
        this.codTra=codT;
    }
    
    public float getCusto(){
        return this.custoEntrega;
    }
    
    public void setCusto(float custo){
        this.custoEntrega=custo;
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("CodEncomenda:").append(this.getCodEncomenda()).append(",").
             append("CodUtilizador:").append(this.getCodUtilizador()).append(",").
             append("CodLoja:").append(this.getCodLoja()).append(",").
             append("CodTransportador:").append(this.getCodTransportador()).append(",").
             append("Custo:").append(this.getCusto()).append(",").
             append(this.getProdutos()).append(",")
             .append("Tempo:").append(this.tempo);
        return sb.toString();
    }
    
    public Encomenda clone(){
        return new EncomendaEfetuada(this);
    }
    
    public boolean equals (Object o){
        if (o==this) return true;
        if (o==null || o.getClass()!=this.getClass()) return false;
        EncomendaEfetuada l=(EncomendaEfetuada) o;
        return super.equals(l) && this.tempo.equals(l.getTempo());
    }
}


