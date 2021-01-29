package Model;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Set;
import java.util.TreeSet;

public class Loja extends AtorSistema implements java.io.Serializable{
    /**
     * Fila de espera da logo, tamnho da fila
     */
    private int filadeEspera;

    /**
     * Estimativa do tempo de espera que uma só encomenda leva
     */
    private Duration tempoEsperaUnico;

    /**
     * Construtor parametrizado de loja
     * @param cod
     * @param nif
     * @param nome
     * @param password
     * @param localizacao
     * @param filadeEspera
     */
    public Loja(String cod, String email, String nif, String nome, String password, Coordenadas localizacao, int filadeEspera, Duration tempo){
        super(cod, email, nif, nome, password, localizacao);
        this.filadeEspera = filadeEspera;
        this.tempoEsperaUnico = tempo;
    }

    /**
     * Construtor por cópia de loja
     * @param loja
     */
    public Loja(Loja loja){
        super(loja);
        setFiladeEspera(loja.getFiladeEspera());
        setTempoEsperaUnico(loja.getTempoEsperaUnico());
    }

    /**
     * Devolve a fila de espera da loja
     * @return
     */
    public int getFiladeEspera() {
        return filadeEspera;
    }

    /**
     * Atualiza a fila de espera da loja
     * @param filadeEspera
     */
    public void setFiladeEspera(int filadeEspera) {
        this.filadeEspera = filadeEspera;
    }

    /**
     * Devolve o tempo de espera que uma só encomenda leva
     * @return
     */
    public Duration  getTempoEsperaUnico(){
        return this.tempoEsperaUnico;
    }

    /**
     * Atualiza o tempo de espera que uma só encomenda leva
     * @param tempo
     */
    public void setTempoEsperaUnico(Duration tempo){
        this.tempoEsperaUnico = tempo;
    }

    /**
     * Devolve o tempo de espera na loja
     * @return
     */
    public Duration tempoEsperaLoja(){
        Duration res = Duration.ZERO;
        for(int i = 1; i <= filadeEspera; i++){
            res = res.plusDays(this.tempoEsperaUnico.toDaysPart());
            res = res.plusHours(this.tempoEsperaUnico.toHoursPart());
            res = res.plusMinutes(this.tempoEsperaUnico.toMinutesPart());
        }
        return res;
    }

    /**
     * Devolve uma cópia da instância loja
     * @return
     */
    @Override
    public Loja clone(){
        return new Loja(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        return super.equals(o) &&
                this.filadeEspera == ((Loja) o).getFiladeEspera() &&
                this.tempoEsperaUnico == ((Loja) o).getTempoEsperaUnico();
    }

    /**
     * Devolve uma representação textual de loja
     * @return
     */
    @Override
    public String toString() {
        return super.toString()+
                ", filadeEspera=" + filadeEspera +
                ", tempoEsperaPorEncomenda=" + tempoEsperaUnico +
                '}';
    }

    /**
     * Método que adiciona o código de uma encomenda a
     * uma coleção de encomendas
     * @param encomenda
     */
    public void addEncomenda(String encomenda){
        super.addEncomenda(encomenda);
    }

}
