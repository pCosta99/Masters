package Models;

import java.io.Serializable;

/**
 * Lojas com informacoes sobre a fila
 *
 */
public class Loja_Queue extends Loja implements Serializable {
    private int tamanho, n_esperas; //varivavel para atualizar o tempo de espera medio
    private double tm; //tempo de espera medio

    public Loja_Queue(String cod, String n, double x, double y, String tipo) {
        super(cod, n, x, y, tipo);
        this.tamanho = 0;
        this.tm = 0;
        this.n_esperas = 0;
    }

    public int getTamanho() {
        return tamanho;
    }

    public void setTamanho(int tamanho) {
        this.tamanho = tamanho;
    }

    public int getTm() {
        return (int) tm;
    }

    /**
     * Atualizar tempo de espera
     * @param time
     */
    private void qstatupdate(int time) {
        this.tm *= n_esperas++;
        this.tm = (tm + time) / n_esperas;
    }

    /**
     * Solicitar nova encomenda
     * @param enc
     */
    public void request(String enc) {
        super.request(enc);
        this.tamanho++;
    }

    /**
     * Sinalizar que está pronta a entregar uma dada encomenda
     * @param enc
     * @param waittime
     * @return
     */
    public Boolean ready(String enc, int waittime) {
        if (super.ready((enc))) {
            this.qstatupdate(waittime);
            tamanho--;
            return true;
        }
        return false;
    }

}
