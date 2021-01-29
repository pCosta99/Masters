package Encomendas;

import java.time.LocalDateTime;
import java.util.ArrayList;

public class EncomendaMedica extends Encomenda{

    public EncomendaMedica(String codTransportadora, String codLoja, String codEncomenda, String codUtilizador, double peso, ArrayList<LinhaEncomenda> listaEnc, LocalDateTime horaPedido, boolean entregue) {
        super(codTransportadora,codLoja,codEncomenda,codUtilizador,peso,listaEnc,horaPedido,entregue);

    }

    public EncomendaMedica(){
       super();
    }

    public EncomendaMedica(EncomendaMedica encomendas) {
        super(encomendas);
    }

    public EncomendaMedica clone(){
        return new EncomendaMedica(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("Código de encomenda: ").append(this.getCodEncomenda()).append("\n")
                .append("Tipo de encomenda : Médica").append("\n")
                .append("Utilizador:").append(this.getCodUtilizador()).append("\n")
                .append("Transportador: ").append(this.getCodTransportadora()).append("\n")
                .append("Loja : ").append(this.getCodLoja()).append("\n")
                .append("Peso da Encomenda: ").append(this.getPeso()).append("\n")
                .append("Entregue: ").append(this.getEntregue()).append("\n")
                .append("Descrição da Encomenda: ").append("\n\n");
        for(LinhaEncomenda l : this.getLinhaEncomenda()){
            sb.append(l.toString()).append("\n");
        }
        return sb.toString();

    }

    public boolean equals(Object o) {
        return super.equals(o);
    }

}


