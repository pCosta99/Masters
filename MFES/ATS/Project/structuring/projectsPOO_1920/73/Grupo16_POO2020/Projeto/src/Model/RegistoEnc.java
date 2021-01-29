package Model;

import java.io.Serializable;
import java.time.LocalDateTime;

public class RegistoEnc implements Serializable {
    private String codUtilizador;
    private String codLoja;
    private String codVol;
    private double custo;
    private double tempo;
    private LocalDateTime data;

    public RegistoEnc(String codUtilizador, String codLoja, String codVol, double custo, double tempo, LocalDateTime data) {
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.codVol = codVol;
        this.custo = custo;
        this.tempo = tempo;
        this.data = data;
    }

    public double getCusto() {
        return custo;
    }

    public void setCusto(double custo) {
        this.custo = custo;
    }

    public double getTempo() {
        return tempo;
    }

    public void setTempo(int tempo) {
        this.tempo = tempo;
    }

    @Override
    public String toString() {
        return "RegistoEnc{" +
                "codUtilizador='" + codUtilizador + '\'' +
                ", codLoja='" + codLoja + '\'' +
                ", codVol='" + codVol + '\'' +
                ", custo=" + custo +
                ", tempo=" + tempo +
                ", data=" + data +
                '}';
    }
}
