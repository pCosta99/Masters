import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Random;

/**
 * A classe AtrasaEncomenda introduz condicionantes ao transporte de encomendas como:
 * mau tempo, trânsito, desvio no transporte, ou engano despropositado no caminho por
 * parte da transportadora.
 * <p>
 * Tudo funciona com base em probabilidades que são calculadas através classe Random.
 */

public class AtrasaEncomenda implements Serializable {
    boolean mauTempo;
    boolean hora_de_ponta;
    boolean desvio;
    boolean enganoCaminho;

    /**
     * Construtor de classe que arrecadará informação acerca dos atrasos que ocorrerão na entrega da encomenda.
     * @param data data da encomenda
     */
    public AtrasaEncomenda(LocalDateTime data) {

        Random gerador = new Random();
        int random_number;

        int mes = data.getMonthValue();
        if ((mes >= 1 && mes <= 4)|| (mes >= 10 && mes <= 11)) {
            random_number  = gerador.nextInt(10);
            if (random_number <= 7) this.mauTempo = true;
        }

        int hora = data.getHour();
        if (hora >= 5 && hora <= 7) {
            random_number = gerador.nextInt(10);
            if (random_number <= 8) this.hora_de_ponta = true;
        }

        random_number = gerador.nextInt(20);
        if (random_number == 1) this.desvio = true;

        random_number = gerador.nextInt(10);
        if (random_number == 1) this.enganoCaminho = true;

    }

    /**
     * Indica se está mau tempo.
     * @return true se estiver mau tempo, false caso contrário
     */
    public boolean isMauTempo() {
        return mauTempo;
    }

    /**
     * Indica se é hora de ponta.
     * @return true se for hora de ponta e false caso contrário
     */
    public boolean isHora_de_ponta() {
        return hora_de_ponta;
    }

    /**
     * Indica se a transportadora fez um desvio.
     * @return true se fez desvio, false caso contrário
     */
    public boolean isDesvio() {
        return desvio;
    }

    /**
     * Indica se houve um engano no caminho pela transportadora.
     * @return true se houve engano no caminho, false caso comtrário
     */
    public boolean isEnganoCaminho() {
        return enganoCaminho;
    }

    /**
     * Calcula, com base nas condicionantes que ocorrerão, o atraso no tempo de entrega.
     * @return minutos de atraso
     */
    public int tempoDeAtraso () {
        int res = 0;
        if (this.mauTempo) res += 5;
        if (this.hora_de_ponta) res+=10;
        if (this.desvio) res += 2;
        if (this.enganoCaminho) res+= 3;

        return res;
    }
}
