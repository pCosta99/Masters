import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

public class ServicoEntrega implements Serializable {


    private Integer classificacao; // int >= 0 && int <= 5
    private EstadoEncomenda estado;
    private LocalDateTime dataNova;
    private LocalDateTime dataProntaASerEntregue;
    private LocalDateTime dataEmAceitacao;
    private LocalDateTime dataEmTransporte;
    private LocalDateTime dataEntregue;
    private double custo;

    /**
     * Construtor
     */
    public ServicoEntrega() {
        this.estado = EstadoEncomenda.NOVA;
        this.dataNova = LocalDateTime.now();
    }



    /**
     * Metodos de instancia
     */
    public Integer getClassificacao() {
        return classificacao;
    }

    public void setClassificacao(int classificacao) {
        if (classificacao >= 0 && classificacao <= 5)
            this.classificacao = classificacao;
    }

    public EstadoEncomenda getEstado() {
        return estado;
    }

    public void setEstado(EstadoEncomenda estado) {
        this.estado = estado;
    }

    public LocalDateTime getDataNova() {
        return dataNova;
    }

    public void setDataNova(LocalDateTime dataNova) {
        this.dataNova = dataNova;
    }

    public LocalDateTime getDataProntaASerEntregue() {
        return dataProntaASerEntregue;
    }

    public void setDataProntaASerEntregue(LocalDateTime dataProntaASerEntregue) {
        this.dataProntaASerEntregue = dataProntaASerEntregue;
    }

    public LocalDateTime getDataEmAceitacao() {
        return dataEmAceitacao;
    }

    public void setDataEmAceitacao(LocalDateTime dataEmAceitacao) {
        this.dataEmAceitacao = dataEmAceitacao;
    }

    public LocalDateTime getDataEmTransporte() {
        return dataEmTransporte;
    }

    public void setDataEmTransporte(LocalDateTime dataEmTransporte) {
        this.dataEmTransporte = dataEmTransporte;
    }

    public LocalDateTime getDataEntregue() {
        return dataEntregue;
    }

    public void setDataEntregue(LocalDateTime dataEntregue) {
        this.dataEntregue = dataEntregue;
    }

    public double getCusto() {
        return custo;
    }

    public void setCusto(double custo) {
        this.custo = custo;
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ServicoEntrega that = (ServicoEntrega) o;
        return Double.compare(that.getCusto(), getCusto()) == 0 &&
                Objects.equals(getClassificacao(), that.getClassificacao()) &&
                getEstado() == that.getEstado() &&
                getDataNova().equals(that.getDataNova()) &&
                Objects.equals(getDataProntaASerEntregue(), that.getDataProntaASerEntregue()) &&
                Objects.equals(getDataEmAceitacao(), that.getDataEmAceitacao()) &&
                Objects.equals(getDataEmTransporte(), that.getDataEmTransporte()) &&
                Objects.equals(getDataEntregue(), that.getDataEntregue());
    }


    public String toString() {
        return "ServicoEntrega{" +
                "classificacao=" + classificacao +
                ", estado=" + estado +
                ", dataNova=" + dataNova +
                ", dataProntaASerEntregue=" + dataProntaASerEntregue +
                ", dataEmAceitacao=" + dataEmAceitacao +
                ", dataEmTransporte=" + dataEmTransporte +
                ", dataEntregue=" + dataEntregue +
                ", custo=" + custo +
                '}';
    }
}
