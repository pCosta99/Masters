import java.io.Serializable;
/**
 * Classe que representa as empresas transportadoras
 */
public class Transportadora extends MeioTransporte implements Serializable {

    private String nif;
    private double taxaDistancia;
    private double taxaPeso;
    private int ocupacao;
    private boolean fazVariasEnc;

    /**
     * Construtor por omissao
     */
    public Transportadora() {
        super();
        this.nif = "";
        this.taxaDistancia = 0.0;
        this.taxaPeso = 0.0;
        this.ocupacao = 0;
        this.fazVariasEnc = false;
    }


    /**
     * Construtor parametrizado
     */
    public Transportadora(String codigo, String nome, GPS gps, double raio, boolean certificado, double velocidade, String nif, double taxaDistancia, double taxaPeso, boolean fazVarEn) {
        super(codigo, nome, gps, raio, certificado, velocidade);
        this.nif = nif;
        this.taxaDistancia = taxaDistancia;
        this.taxaPeso = taxaPeso;
        this.ocupacao = 0;
        this.fazVariasEnc = fazVarEn;
    }

    /**
     * Construtor por cópia
     */
    public Transportadora(Transportadora t) {
        super(t);
        this.nif = t.getNif();
        this.taxaDistancia = t.getTaxaDistancia();
        this.taxaPeso = t.getTaxaPeso();
        this.ocupacao = t.getOcupacao();
        this.fazVariasEnc = t.isFazVariasEnc();
    }

    /**
     * Metodos de instancia
     */

    public String getNif() {
        return nif;
    }

    public double getTaxaDistancia() {
        return taxaDistancia;
    }

    public double getTaxaPeso() {
        return taxaPeso;
    }

    public int getOcupacao() {
        return this.ocupacao;
    }

    public boolean isFazVariasEnc() {
        return this.fazVariasEnc;
    }


    public void setNif(String nif) {
        this.nif = nif;
    }

    public void setTaxaDistancia(double taxaDistancia) {
        this.taxaDistancia = taxaDistancia;
    }


    public void setTaxaPeso(double taxaPeso) {
        this.taxaPeso = taxaPeso;
    }

    public void setOcupacao(int ocup) {
        this.ocupacao = ocup;
    }

    public void setFazVariasEnc(boolean fazVariasEnc) {
        this.fazVariasEnc = fazVariasEnc;
    }



    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Transportadora that = (Transportadora) o;
        return Double.compare(that.getTaxaDistancia(), getTaxaDistancia()) == 0 &&
                Double.compare(that.getTaxaPeso(), getTaxaPeso()) == 0 &&
                getOcupacao() == that.getOcupacao() &&
                isFazVariasEnc() == that.isFazVariasEnc() &&
                getNif().equals(that.getNif());
    }


    public String toString() {
        return "Transportadora{" +
                "nif='" + nif + '\'' +
                ", taxaDistancia=" + taxaDistancia +
                ", taxaPeso=" + taxaPeso +
                ", ocupacao=" + ocupacao +
                ", fazVariasEnc=" + fazVariasEnc +
                '}';
    }

    public Transportadora clone() {
        return new Transportadora(this);
    }


    public double calculaPrecoTransporte(double peso, double distLoja, double distUserLoja, double tempoEspera) {
        double distTotal = distLoja + distUserLoja;
        return ((taxaDistancia * distTotal) + peso) - tempoEspera;
    }
}





