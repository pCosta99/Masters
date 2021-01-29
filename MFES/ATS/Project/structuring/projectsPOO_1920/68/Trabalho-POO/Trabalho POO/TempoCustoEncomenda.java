import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

public class TempoCustoEncomenda implements Comparable<TempoCustoEncomenda>, Serializable {
    double tempo;
    double custo;
    LocalDateTime dataEntrega;

    public TempoCustoEncomenda () {
        this.tempo = 0;
        this.custo = 0;
        this.dataEntrega = LocalDateTime.now();
    }

    public TempoCustoEncomenda (double tempo, double custo, LocalDateTime data) {
        this.tempo = tempo;
        this.custo = custo;
        this.dataEntrega = data;
    }

    public TempoCustoEncomenda (TempoCustoEncomenda tce) {
        this.tempo = tce.getTempo();
        this.custo = tce.getCusto();
        this.dataEntrega = tce.getData();
    }

    public double getTempo() {
        return tempo;
    }

    public void setTempo(double tempo) {
        this.tempo = tempo;
    }

    public double getCusto() {
        return custo;
    }

    public void setCusto(double custo) {
        this.custo = custo;
    }

    public LocalDateTime getData() {
        return dataEntrega;
    }

    public void setData(LocalDateTime data) {
        this.dataEntrega = data;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Tempo: ").append(this.tempo).append("   Custo: ").append(this.custo).append("   Data: ").append(this.dataEntrega.toString());
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TempoCustoEncomenda that = (TempoCustoEncomenda) o;
        return tempo == that.tempo &&
                Double.compare(that.custo, custo) == 0 &&
                Objects.equals(dataEntrega, that.dataEntrega);
    }

    @Override
    public int hashCode() {
        return Objects.hash(tempo, custo, dataEntrega);
    }


    public boolean isAfter(LocalDateTime inicio){
        return this.dataEntrega.isAfter(inicio);
    }

    public boolean isBefore(LocalDateTime fim){
        return this.dataEntrega.isBefore(fim);
    }

    public TempoCustoEncomenda clone() {
        return new TempoCustoEncomenda(this);
    }

    public LocalDateTime dataSaida() {
        return this.dataEntrega.minusMinutes((long)this.tempo);
    }

    /**
     * A ordem natural de comparação de objetos desta class está relacionada com a data.
     */
    public int compareTo(TempoCustoEncomenda tce) {
        return this.dataEntrega.compareTo(tce.getData());
    }
}
