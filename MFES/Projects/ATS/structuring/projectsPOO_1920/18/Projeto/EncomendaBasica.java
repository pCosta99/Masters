import java.util.Map;

public class EncomendaBasica extends Encomenda
{
    
    public EncomendaBasica(){
        super();
    }
    
    public EncomendaBasica(String codE, String codU, String codL,float peso, Map<String,LinhaEncomenda> prod){
        super(codE,codU,codL,peso,prod);
    }
    
    public EncomendaBasica (Encomenda e){
        super(e.getCodEncomenda(),e.getCodUtilizador(),e.getCodLoja(),e.getPeso(),e.getProdutos());
    }
    
    public Encomenda clone(){
        return new EncomendaBasica(this);
    }
}
