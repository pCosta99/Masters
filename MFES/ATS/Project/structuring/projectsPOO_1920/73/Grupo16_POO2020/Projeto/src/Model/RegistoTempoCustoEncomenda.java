package Model;

import java.time.LocalDateTime;

public class RegistoTempoCustoEncomenda {
    private String registoEnc;
    private double custo;
    private double tempo;
    private LocalDateTime data;

    public RegistoTempoCustoEncomenda(String registoEnc, double custo, double tempo, LocalDateTime data) {
        this.registoEnc = registoEnc;
        this.custo = custo;
        this.tempo = tempo;
        this.data = data;
    }

    public String getRegistoEnc() {
        return registoEnc;
    }

    public void setRegistoEnc(String registoEnc) {
        registoEnc = registoEnc;
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
}
