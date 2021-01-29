package Models;

import java.io.Serializable;

public class AceitaEncomenda implements Serializable {
    private String e;

    /**
     * Construtor por cópia.
     */
    public AceitaEncomenda() {
        this.e = "n/a";
    }

    /**
     * Construtor parametrizado.
     * @param e Recebe uma String representante do código de uma Encomenda.
     */
    public AceitaEncomenda(String e) {
        this.e = e;
    }

    /**
     * Construtor por cópia.
     * @param ae Recebe um objeto da classe AceitaEncomenda.
     */
    public AceitaEncomenda(AceitaEncomenda ae) {
        this.e = ae.getE();
    }

    /**
     * Método que dá o código de uma Encomenda.
     * @return Devolve uma String do código.
     */
    public String getE() {
        return this.e;
    }

    /**
     * Método que define o código de uma Encomenda.
     * @param e Recebe uma String do código.
     */
    public void setE(String e) {
        this.e = e;
    }

    /**
     * Função que traduz a classe AceitaEncomenda.
     * @return Devolve uma String com a respetiva tradução.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código da TrazAqui.Encomenda:").append(this.e);
        return sb.toString();
    }

    /**
     * Função que verifica se o objeto recebido é idêntico ao da classe AceitaEncomenda.
     * @param o Recebe um objeto.
     * @return Devolve um boolean que faz a respetiva verificação.
     */
    @Override
    public boolean equals(Object o) {
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        AceitaEncomenda ae = (AceitaEncomenda) o;
        return ae.getE().equals(this.e);
    }

    /**
     * Função que faz um clone da classe AceitaEncomenda.
     * @return Devolve esse clone.
     */
    @Override
    public AceitaEncomenda clone() {
        return new AceitaEncomenda(this);
    }
}
