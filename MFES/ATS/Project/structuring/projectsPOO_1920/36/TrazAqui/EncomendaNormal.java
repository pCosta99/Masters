
/**
 * Write a description of class EncomendaNormal here.
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.*;

public class EncomendaNormal extends Encomenda {
    
    public EncomendaNormal() {
        super();
    }
    
    public EncomendaNormal(String codigoEncomenda, String codigoUtilizador, String codigoLoja, double peso, List<Produto>produtos, EstadoEncomenda estadoEncomenda) {
       super(codigoEncomenda, codigoUtilizador, codigoLoja, peso, produtos, estadoEncomenda);
    }
    
    public EncomendaNormal(EncomendaNormal e) {
        super(e);
    }
    
    public boolean equals(Object obj) {
        if(this == obj) return true;
        if((obj == null) || (this.getClass() != obj.getClass())) return false;
        
        Encomenda e = (Encomenda) obj;
        return super.equals(e);
    }
        
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomendas: ").append("\n");
        sb.append(" - Código encomenda: ").append(this.getCodigoEncomenda()).append("\n");
        sb.append(" - Código utilizador: ").append(this.getCodigoUtilizador()).append("\n");
        sb.append(" - Código loja: ").append(this.getCodigoLoja()).append("\n");
        sb.append(" - Código transportador: ").append(this.getCodigoTransportador()).append("\n");
        sb.append(" - Peso: ").append(this.getPeso()).append("\n");
        sb.append(" - Produtos: ").append(this.getProdutos().toString());
        sb.append(" - Estado de encomenda: ").append(this.getEstadoEncomenda().toString());
        sb.append(" - Distancia: ").append(this.getDistancia());
        sb.append(" - Duração de entrega: ").append(this.getDuracaoEntrega());
        sb.append(" - Preço de transporte: ").append(this.getPrecoTransporte());
        sb.append(" - Data de encomenda: ").append(this.getDataEncomenda());
        sb.append(" - Data de entrega: ").append(this.getDataEntrega());
        return sb.toString();
    }
    
    public EncomendaNormal clone() {
        return new EncomendaNormal(this);
    } 
    
}


