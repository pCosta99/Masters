/**
 * Classe meio transporte de onde deriva voluntário e transportadora
 */
public abstract class MeioTransporte extends Entidade {
    private double raio;
    private double velocidade;
    private boolean disponivel;
    private boolean aceitoMeds;
    private boolean certificado;

    /**
     * Construtor por omissao
     */
    public MeioTransporte() {
        super();
        this.raio = 0.0;
        this.velocidade = 0.0;
        this.disponivel = false;
        this.aceitoMeds = false;
        this.certificado = false;
    }

    /**
     * Construtor parametrizado
     */
    public MeioTransporte(String codigo, String nome, GPS gps, double raio, boolean certificado, double velocidade) {
        super(codigo, nome, gps);
        this.raio = raio;
        this.velocidade = velocidade;
        this.disponivel = false;
        this.aceitoMeds = false;
        this.certificado = certificado;
    }

    /**
     * Construtor por cópia
     */
    public MeioTransporte(MeioTransporte t) {
        super(t);
        this.raio = t.getRaio();
        this.velocidade = t.getVelocidade();
        this.disponivel = t.isDisponivel();
        this.aceitoMeds = t.aceitoTransporteMedicamentos();
        this.certificado = t.isCertificado();
    }

    /**
     * Devolve o raio de transporte
     * @return Raio
     */
    public double getRaio() {
        return raio;
    }

    /**
     * Devolve a velocidade do meio de transporte
     * @return Velocidade
     */
    public double getVelocidade() {
        return velocidade;
    }

    /**
     * Informa se o meio de transporte está ou não disponível
     * @return true se estiver disponivel, false caso contrário
     */
    public boolean isDisponivel() {
        return disponivel;
    }


    /**
     * Informa se o meio de transporte aceita fazer transporte de medicamentos
     * @return true se quiser, false caso contrário
     */
    public boolean aceitoTransporteMedicamentos() {
        return aceitoMeds;
    }


    public boolean isCertificado() {
        return certificado;
    }


    /**
     * Setters
     */

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public void setVelocidade(double velocidade) {
        this.velocidade = velocidade;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }


    public void setAceitaMedicamentos(boolean state) {
        this.aceitoMeds = state;
    }

    public void setCertificado(boolean certificado) {
        this.certificado = certificado;
    }



    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        MeioTransporte that = (MeioTransporte) o;
        return Double.compare(that.getRaio(), getRaio()) == 0 &&
                Double.compare(that.getVelocidade(), getVelocidade()) == 0 &&
                isDisponivel() == that.isDisponivel() &&
                aceitoMeds == that.aceitoMeds &&
                isCertificado() == that.isCertificado();
    }


    public String toString() {
        return "MeioTransporte{" +
                "raio=" + raio +
                ", velocidade=" + velocidade +
                ", disponivel=" + disponivel +
                ", aceitoMeds=" + aceitoMeds +
                ", certificado=" + certificado +
                '}';
    }

    public abstract MeioTransporte clone();
}
