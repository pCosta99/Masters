import java.io.Serializable;

public class Estafeta extends Entidade implements Serializable {
    private double raio;
    private boolean certificado;
    private boolean voluntario;

    public Estafeta(){
        super();
        this.raio = 0;
        this.certificado = false;
        this.voluntario = true;
    }

    public Estafeta(String umCodigo, String umNome, GPS umaLocalizacao, double umRaio, boolean umCertificado, boolean umVoluntario){
        super(umCodigo, umNome, umaLocalizacao);
        this.raio = umRaio;
        this.certificado = umCertificado;
        this.voluntario = umVoluntario;
    }

    /**
     * Método de criação de Empresas para os Logs fornecidos
     * @param umCodigo
     * @param umNome
     * @param umaLocalizacao
     */
    public Estafeta(String umCodigo, String umNome, GPS umaLocalizacao, double umRaio){
        super(umCodigo, umNome, umaLocalizacao);
        this.raio = umRaio;
        this.certificado = false;
        this.voluntario = true;
    }

    public Estafeta(Estafeta umEstafeta){
        super(umEstafeta);
        this.raio = umEstafeta.getRaio();
        this.certificado = umEstafeta.isCertificado();
        this.voluntario = umEstafeta.isVoluntario();
    }

    public Estafeta clone(){
        return new Estafeta(this);
    }

    public boolean isCertificado() {
        return this.certificado;
    }

    public boolean isVoluntario() {
        return this.voluntario;
    }

    public double getRaio(){
        return this.raio;
    }

    public void setCertificado(boolean novoCertificado) {
        this.certificado = novoCertificado;
    }

    public void setVoluntario(boolean novoVoluntario) {
        this.voluntario = novoVoluntario;
    }

    public void setRaio(double novoRaio){
        this.raio = novoRaio;
    }

    public String toString(){
        return super.toString() +
                "\n\t Tem Certificado de Produtos Médicos = " + this.certificado +
                "\n\t É voluntário = " + this.voluntario +
                '\n' + '}';
    }

    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || this.getClass() != o.getClass())
            return false;

        Estafeta e = (Estafeta) o;
        return super.equals(e) &&
                this.certificado == e.isCertificado() &&
                this.voluntario == e.isVoluntario();
    }

    public String toCSV(){
        return super.toCSV() + ',' + this.raio + ',' + this.certificado + ',' + this.voluntario;
    }

    public String toLog(){
        return super.toLog() + ',' + this.raio;
    }
}

