import java.time.Duration;
import java.io.Serializable;
import java.time.LocalDateTime;
public class RegistoEntregas implements Comparable<RegistoEntregas>, Serializable{
    private double custo;
    private String codTransportador;
    private Duration timeElapsed;
    private Encomenda enc;
    /**
     * Construtor por omissao da classe
     */
    public RegistoEntregas(){
        this.custo = 0.0;
        this.codTransportador = "";
        this.timeElapsed = Duration.ZERO;
        this.enc = null;
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros double nc, String codT, Encomenda nEnc,Duration z
     */ 
    public RegistoEntregas(double nc, String codT, Encomenda nEnc,Duration z){
        this.custo = nc;
        this.codTransportador = codT;
        this.enc = nEnc.clone();
        this.timeElapsed = z;
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */ 
    public RegistoEntregas(RegistoEntregas p){
        this.custo = p.getCusto();
        this.codTransportador = p.getCodTransportador();
        
        this.enc = p.getEnc();
        this.timeElapsed = p.getDuration();
    }
    /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Custo: ").append(this.custo).append("\n");
        sb.append("Codigo de transportadora/voluntario: ").append(this.codTransportador).append("\n");
        sb.append("Tempo de entrega: ").append(this.timeElapsed).append("\n");
        sb.append("Encomenda: \n").append(this.enc).append("\n");

        return sb.toString();
    }
    /**
     * Metodo que devolve em forma de String algumas das variaveis da classe
     * @return String resutante
     */
    public String info(){
        StringBuilder sb = new StringBuilder();
        sb.append("Custo: ").append(this.custo).append("\n");
        sb.append("Codigo de transportadora/voluntario: ").append(this.codTransportador).append("\n");
        sb.append("Tempo de entrega: ").append(this.timeElapsed).append("\n");
        return sb.toString();
    }
    /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object o){
        if(this == o)
            return true;
        if(this.getClass() == o.getClass())
            return false;
        RegistoEntregas p = (RegistoEntregas) o;
        return this.custo == p.getCusto() && this.codTransportador.equals(p.getCodTransportador()) && this.enc.equals(p.getEnc());
    }  
    /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public RegistoEntregas clone(){
        return new RegistoEntregas(this);
    }
    /**
     * Devolve o double custo da classe
     * @return double custo
     */
    public double getCusto(){
        return this.custo;
    }
    /**
     * Devolve a String codTransportadora da classe
     * @return String codTransportadora
     */
    public String getCodTransportador(){
        return this.codTransportador;
    }
    /**
     * Devolve a Encoemnda enc da classe
     * @return Encomenda enc
     */
    public Encomenda getEnc(){
        return this.enc.clone();
    }
    /**
     * Atualiza o double custo da classe
     * @param nc novo custo da classe
     */
    public void setCusto(double nc){
        this.custo = nc;
    }
    /**
     * Atualiza a String codTransportadora da classe
     * @param ct novo codTransportadora da classe
     */
    public void setCodTransportador(String ct){
        this.codTransportador = ct;
    }
    /**
     * Atualiza a Encomenda enc da classe
     * @param nenc nova enc da classe
     */
    public void setEnc(Encomenda nenc){
        this.enc = nenc.clone();
    }
    /**
     * Devolve a Duration timeElapsed da classe
     * @return Duration timeElapsed
     */
    public Duration getDuration(){
        return this.timeElapsed;
    }
    /**
     * Atuliza a Duration timeElapsed da classe
     * @param p nova Duration da classe
     */
    public void setTimeOfDelivery(Duration p){
        this.timeElapsed = p;
    }
    /**
     * Metodo que faz a comparaçao entre dois objetos da classe
     * @param RegistoEntregas b
     * @return int resultante de comparaçao
     */
    public int compareTo(RegistoEntregas b){
        return this.codTransportador.compareTo(b.getCodTransportador());
    }
}
