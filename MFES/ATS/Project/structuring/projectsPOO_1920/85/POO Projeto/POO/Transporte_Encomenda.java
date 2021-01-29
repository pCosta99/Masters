import java.time.LocalDateTime;

public class Transporte_Encomenda extends Encomenda implements Transporte_EncomendaI{
    // Variaveis de instancia
    private LocalDateTime inicioTransporte;
    private LocalDateTime fimTransporte;
    private double custoTransporte;

    /**
     * Construtores para objetos da classe Transporte_Encomenda
     */
    public Transporte_Encomenda(){
        super();
    }

    public Transporte_Encomenda (EncomendaI e){
        super((Encomenda) e);
    }

    public Transporte_Encomenda(EncomendaI e, LocalDateTime inicioTransporte, LocalDateTime fimTransporte, double custoTransporte) {
        super((Encomenda) e);
        this.inicioTransporte = inicioTransporte;
        this.fimTransporte = fimTransporte;
        this.custoTransporte = custoTransporte;
    }

    public Transporte_Encomenda(Transporte_Encomenda te){
        super(te);
        this.inicioTransporte = te.getInicioTransporte();
        this.fimTransporte = te.getFimTransporte();
        this.custoTransporte = te.getCustoTransporte();
    }

    /**
     * Metodos gets e sets,
     * clone, equals e toString
     */
    public LocalDateTime getInicioTransporte() {
        return inicioTransporte;
    }

    public void setInicioTransporte(LocalDateTime inicioTransporte) {
        this.inicioTransporte = inicioTransporte;
    }

    public LocalDateTime getFimTransporte() {
        return fimTransporte;
    }

    public void setFimTransporte(LocalDateTime fimTransporte) {
        this.fimTransporte = fimTransporte;
    }

    public double getCustoTransporte() {
        return custoTransporte;
    }

    public void setCustoTransporte(double custoTransporte) {
        this.custoTransporte = custoTransporte;
    }

    public Transporte_Encomenda clone(){
        return new Transporte_Encomenda(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Transporte Encomenda\n")
                .append(super.toString());
        if(this.inicioTransporte == null)
            sb.append("CustoTransporte: ").append(this.custoTransporte).append("\n");
        else {
            if(this.fimTransporte == null){
                sb.append("InicioTransporte: ").append(this.inicioTransporte).append("\n")
                        .append("CustoTransporte: ").append(this.custoTransporte).append("\n");
            }
            else {
                sb.append("InicioTransporte: ").append(this.inicioTransporte).append("\n")
                        .append("FimTransporte: ").append(this.fimTransporte).append("\n");
                        if(this.custoTransporte != 0)
                            sb.append("CustoTransporte: ").append(this.custoTransporte).append("\n");
            }
        }
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Transporte_Encomenda te = (Transporte_Encomenda) o;
        return Double.compare(te.getCustoTransporte(), this.custoTransporte) == 0 &&
                te.getInicioTransporte().equals(this.inicioTransporte) &&
                te.getFimTransporte().equals(this.fimTransporte);
    }

}
