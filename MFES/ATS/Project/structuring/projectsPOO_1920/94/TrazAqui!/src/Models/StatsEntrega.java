package Models;

/**
 * Informacoes de uma encomenda
 */

import java.io.Serializable;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class StatsEntrega implements Serializable {

    private LocalDateTime hora_saida;//Define a hora de saída

    private LocalDateTime hora_entrada;//Define a hora de entrada

    private double faturado;

    /**
     * Construtor da classe
     * @param hora_entrada hora de saida do agente
     */
    public StatsEntrega(LocalDateTime hora_entrada) {
        this.hora_entrada = hora_entrada;
        hora_saida = null;
        faturado = 0;
    }

    /**
     * Construtor de classe
     * @param s
     */
    public StatsEntrega(StatsEntrega s) {
        this.hora_entrada = s.hora_entrada;
        hora_saida = s.hora_saida;
        faturado = s.faturado;
    }

    /**
     * Construtor de classe
     * @param hora_entrada hora de saida do agente
     * @param faturado preço da entrega
     */
    public StatsEntrega(LocalDateTime hora_entrada, double faturado) {
        this.hora_entrada = hora_entrada;
        this.faturado = faturado;
        hora_saida = null;
    }


    public StatsEntrega clone() {
        return new StatsEntrega(this);
    }


    public LocalDateTime getHora_saida() {
        return hora_saida;
    }

    public void setHora_saida(LocalDateTime hora_saida) {
        this.hora_saida = hora_saida;
    }

    public LocalDateTime getHora_entrada() {
        return hora_entrada;
    }

    public void setHora_entrada(LocalDateTime hora_entrada) {
        this.hora_entrada = hora_entrada;
    }

    public double getFaturado() {
        return faturado;
    }

    public void setFaturado(double faturado) {
        this.faturado = faturado;
    }


    private String tempo() {
        Duration diff = Duration.between(this.hora_entrada, this.hora_saida);
        return String.format("%d:%02d:%02d",
                diff.toHours(),
                diff.toMinutesPart(),
                diff.toSecondsPart());
    }

    public String toString() {
        DateTimeFormatter f = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        String hsaida = this.hora_saida.format(f);
        String hentrada = this.hora_entrada.format(f);

        return "\nHora de entrada: " + hentrada + " | Hora da saida: " + hsaida +
                "\nTempo total da entrega: " + tempo() +
                "\nTotal faturado: " + this.faturado + "\n";
    }
}
