import java.util.Map;
import java.util.TreeMap;
import java.time.LocalDateTime;
import java.time.Duration;
import java.io.Serializable;
public class RegistosTransporte implements Comparable<RegistosTransporte>, Serializable{
    private String user;
    private LocalDateTime horaInicial;
    private LocalDateTime horaEntrega;
    private double custo;
    private double kilometros;
    private Encomenda enc;            
    /**
     * Construtor por omissao da classe
     */
    public RegistosTransporte(){
        this.user = "";
        this.horaInicial = LocalDateTime.now();
        this.horaEntrega = LocalDateTime.now();
        this.custo = 0.0;
        this.kilometros = 0.0;
        this.enc = new Encomenda();
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros String u, LocalDateTime h1, LocalDateTime h2, double c, double kmPercorridos, Encomenda e
     */ 
    public RegistosTransporte(String u, LocalDateTime h1, LocalDateTime h2, double c, double kmPercorridos, Encomenda e){
        this.user = u;
        this.horaInicial = h1;
        this.horaEntrega = h2;
        this.custo = c;
        this.kilometros = kmPercorridos;
        this.enc = e.clone();
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
    public RegistosTransporte(RegistosTransporte r){
        this.user = r.getUtilizador();
        this.horaInicial = r.getHoraInicial();
        this.horaEntrega = r.getHoraEntrega();
        this.custo = r.getCusto();
        this.kilometros = r.getKilometros();
        this.enc = r.getEncomenda();   
    }

    /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();   
        sb.append("Utilizador: ").append(this.user).append("\n");
        sb.append("horaInicial: ").append(this.horaInicial).append("\n");
        sb.append("horaEntrega: ").append(this.horaEntrega).append("\n");
        sb.append("custo: ").append(this.custo).append("\n");
        sb.append("kilometros: ").append(this.kilometros).append("\n");
        sb.append("enc: ").append(this.enc).append("\n");
        return sb.toString();
    }
    /**
     * Metodo que devolve em forma de String algumas das variaveis da classe
     * @return String resutante
     */
    public String info(){
        StringBuilder sb = new StringBuilder();   
        sb.append("Utilizador: ").append(this.user).append("\n");
        sb.append("horaInicial: ").append(this.horaInicial).append("\n");
        sb.append("horaEntrega: ").append(this.horaEntrega).append("\n");
        sb.append("custo: ").append(this.custo).append("\n");
        sb.append("kilometros: ").append(this.kilometros).append("\n");
        return sb.toString();        
    }
    /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object o){
        if(this == o)
            return true;
        if (o == null || this.getClass() != o.getClass())
            return false;
        RegistosTransporte rp = (RegistosTransporte) o;
        return this.user.equals(rp.getUtilizador()) && this.enc.equals(rp.getEncomenda());
    }
    /**
     * Devolve o LocalDateTime horaEntrega da classe
     * @return LocalDateTime horaEntrega
     */
    public LocalDateTime getHoraEntrega(){
        return this.horaEntrega;
    }
    /**
     * Devolve o LocalDateTime horaInicial da classe
     * @return LocalDateTime horaInicial
     */
    public LocalDateTime getHoraInicial(){
        return this.horaInicial;
    }
    /**
     * Devolve a String user da classe
     * @return String user
     */
    public String getUtilizador(){
        return this.user;
    }
    /**
     * Devolve o double custo da classe
     * @return double custo
     */
    public double getCusto(){
        return this.custo;
    }
    /**
     * Devolve o double kilometros da classe
     * @return double kilometros
     */
    public double getKilometros(){
        return this.kilometros;
    }
    /**
     * Devolve a Encomenda enc da classe
     * @return Encomenda enc
     */
    public Encomenda getEncomenda(){
        return this.enc.clone();
    }
    /**
     * Atualiza o LocalDateTime horaEntrega da classe
     * @param t nova horaEntrega da classe
     */
    public void setHoraEntrega(LocalDateTime t){
        this.horaEntrega = t;
    }
    /**
     * Atualiza o LocalDateTime horaInicial da classe
     * @param t nova horaInicial da classe
     */
    public void setHoraInicial(LocalDateTime t){
        this.horaInicial = t;
    }
    /**
     * Atualiza o double kilometros da classe
     * @param kms novo kilometros da classe
     */
    public void setKilometros(double kms){
        this.kilometros = kms;
    }
    /**
     * Atualiza a String user da classe
     * @param u novo user da classe
     */
    public void setCodigoUser(String u){
        this.user = u;
    }
    /**
     * Atualiza a Encomenda enc da classe
     * @param en nova enc da classe
     */
    public void setEncomenda(Encomenda en){
        this.enc = en.clone();
    }
    /**
     * Atualiza o double custo da classe
     * @param c novo custo da classe
     */
    public void setCusto(double c){
        this.custo = c;
    }
      /**
     * Metodo que faz a comparaçao entre dois objetos da classe
     * @param RegistoTransporte b
     * @return int resultante de comparaçao
     */
    public int compareTo(RegistosTransporte r){
        return this.horaEntrega.compareTo(r.getHoraEntrega());
    }
    /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public RegistosTransporte clone(){
        return new RegistosTransporte(this);
    }

}
