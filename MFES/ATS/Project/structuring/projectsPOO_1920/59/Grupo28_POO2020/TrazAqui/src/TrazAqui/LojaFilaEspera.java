package TrazAqui;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class LojaFilaEspera extends Loja implements Entrada {
    /**
     * Variaveis de instancia
     */
    private int filaEspera;
    private double tempoEspera;

    /**
     * Retorna a fila de espera
     * @return int 
     */
    public int getListaEspera() {
        return filaEspera;
    }

    /**
     * Define a fila de espera 
     * @param filaEspera int 
     */
    public void setListaEspera(int filaEspera) {
        this.filaEspera = filaEspera;
    }

    /**
     * Retorna o tempo de espera
     * @return double 
     */
    public double getTempoEspera() {
        return tempoEspera;
    }

    /**
     * Define o tempo de espera
     * @param tempoEspera double
     */
    public void setTempoEspera(double tempoEspera) {
        this.tempoEspera = tempoEspera;
    }

    /**
     * Construtor vazio
     */
    public LojaFilaEspera() {
        super();
        this.filaEspera = ThreadLocalRandom.current().nextInt(11);
        this.tempoEspera = ThreadLocalRandom.current().nextDouble(5.0,30.0);
    }

    /**
     * Construtor parametrizado 
     * @param cod String
     * @param nome String
     * @param localizacao GPS
     * @param l List<Encomenda>
     * @param lis int 
     * @param tempo double 
     */
    public LojaFilaEspera(String cod, String nome, GPS localizacao, List<Encomenda> l, int lis, double tempo) {
        super(cod,nome,localizacao,l);
        this.setListaEspera(lis);
        this.tempoEspera = tempo;
    }

    /**
     * Construtor por copia 
     * @param j LojaFilaEspera
     */
    public LojaFilaEspera(LojaFilaEspera j) {
        super(j);
        this.filaEspera = j.getListaEspera();
        this.tempoEspera = j.getTempoEspera();
    }

    /**
     * Retorna uma copia da class que a chama
     * @return LojaFilaEspera
     */
    public LojaFilaEspera clone() {
        return new LojaFilaEspera(this);
    }

    /**
     * Retorna os da loja com fila de espera em formato string
     * @return String
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("LojaFilaEspera {");
        sb.append("cod = ").append(this.getCod());
        sb.append(", nome = ").append(this.getNome());
        sb.append(", localizacao = ").append(this.getLocalizacao().toString());
        sb.append(", lista de espera = ").append(this.filaEspera);
        sb.append(", tempo de espera = ").append(this.tempoEspera);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Compara o objeto recebido com o que chama
     * @param o Object
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LojaFilaEspera)) return false;
        if (!super.equals(o)) return false;
        LojaFilaEspera that = (LojaFilaEspera) o;
        return Double.compare(that.getTempoEspera(), getTempoEspera()) == 0 &&
                this.filaEspera == that.getListaEspera();
    }

    /**
     * Retorna o tamanho da lista de espera
     * @return int
     */
    public int getTamanhoListaEspera() {
        return this.filaEspera;
    }

    /**
     * Retorna o nome da loja com fila de espera
     * @return String
     */
    public String toStringNome() {
        return "LojaFilaEspera";
    }
}
