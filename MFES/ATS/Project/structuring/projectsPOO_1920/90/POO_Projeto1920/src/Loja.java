import java.io.Serializable;

public class Loja extends Entidade implements Serializable {
    private boolean infoFilas;
    private int tempoAtendimentoPorPessoa; // em segundos
    private int pessoasEmEspera;

    /**
     * Construtor por omissão
     */
    public Loja() {
        super();
        this.infoFilas = false;
        this.tempoAtendimentoPorPessoa = 0;
        this.pessoasEmEspera = 0;
    }

    /**
     * Construtor parametrizado
     */

    public Loja(String codigo, String nome, GPS gps, boolean infoFilas) {
        super(codigo, nome, gps);
        this.infoFilas = infoFilas;
        this.tempoAtendimentoPorPessoa = 0;
        this.pessoasEmEspera = 0;
    }

    public Loja(String codigo, String nome, GPS gps) {
        super(codigo, nome, gps);
        this.infoFilas = false;
        this.tempoAtendimentoPorPessoa = 0;
        this.pessoasEmEspera = 0;
    }

    /**
     * Construtor de cópia
     */
    public Loja(Loja l) {
        super(l);
        this.infoFilas = l.isInfoFilas();
        this.tempoAtendimentoPorPessoa = l.getTempoAtendimentoPorPessoa();
        this.pessoasEmEspera = l.getPessoasEmEspera();
    }

    /**
     * Getters
     */

    public boolean isInfoFilas() {
        return infoFilas;
    }

    public int getTempoAtendimentoPorPessoa() {
        return tempoAtendimentoPorPessoa;
    }

    public int getPessoasEmEspera() {
        return pessoasEmEspera;
    }



    /**
     * Setters
     */

    public void setInfoFilas(boolean infoFilas) {
        this.infoFilas = infoFilas;
    }

    public void setTempoAtendimentoPorPessoa(int tempoAtendimentoPorPessoa) {
        this.tempoAtendimentoPorPessoa = tempoAtendimentoPorPessoa;
    }

    public void setPessoasEmEspera(int pessoasEmEspera) {
        this.pessoasEmEspera = pessoasEmEspera;
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Loja loja = (Loja) o;
        return isInfoFilas() == loja.isInfoFilas() &&
                getTempoAtendimentoPorPessoa() == loja.getTempoAtendimentoPorPessoa() &&
                getPessoasEmEspera() == loja.getPessoasEmEspera();
    }


    public String toString() {
        return "Loja{" +
                "infoFilas=" + infoFilas +
                ", tempoAtendimentoPorPessoa=" + tempoAtendimentoPorPessoa +
                ", pessoasEmEspera=" + pessoasEmEspera +
                '}';
    }

    /**
     * Clona uma loja
     *
     * @return Loja clone
     */
    public Loja clone() {
        return new Loja(this);
    }
}
