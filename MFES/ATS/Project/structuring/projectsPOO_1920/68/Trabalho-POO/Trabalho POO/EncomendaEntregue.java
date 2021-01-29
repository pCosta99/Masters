import java.io.Serializable;

public class EncomendaEntregue extends Encomenda implements Serializable {
    private TempoCustoEncomenda tce;


    /**
     * Construtor standard duma EncomendaEntregue. 
     * @param e Encomenda que acabou de ser entregue ao utilizador.
     * @param tce Informação acerca da entrega.
     */
    public EncomendaEntregue(Encomenda e, TempoCustoEncomenda tce) {
        super(e);
        this.tce = tce.clone();
    }

    /**
     * Construtor por cópia.
     * @param enc Encomenda original.
     */
    public EncomendaEntregue(EncomendaEntregue enc) {
        super(enc);
        this.tce = enc.getTCE();
    }

    /**
     * Obter informações acerca do transporte desta encomenda.
     */
    public TempoCustoEncomenda getTCE() {
        return tce.clone();
    }

    public void setTCE(TempoCustoEncomenda tce) {
        this.tce = tce.clone();
    }

    public double tempoEntrega() {
        return this.tce.getTempo();
    }

    public EncomendaEntregue clone() {
        return new EncomendaEntregue(this);
    }

    public int compareTo(EncomendaEntregue e) {
        // queremos devolver negativo quando queremos que apareça primeiro, ou seja, queremos que dê negativo para as encomendas mais recentes.
        if (this.tce.dataSaida().isAfter(e.getTCE().dataSaida())) return -1;
        else return 1;
    }

    
}