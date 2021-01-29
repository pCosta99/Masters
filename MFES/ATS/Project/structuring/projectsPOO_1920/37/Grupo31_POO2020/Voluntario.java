
/**
 * Escreva a descrição da classe Voluntarios aqui.
 * 
 * @author (seu nome)
 * @version (número de versão ou data)
 */
import java.io.*;
public class Voluntario extends Join implements Certificados,Serializable{
    private String nomeVoluntario;
    private boolean estado;
    private double velocidade;
    private double raio;
    private String codV; //codigo do voluntario
    private boolean certificado; //se pode transportar medicamentos
    private int numViagens;
    private double classificacao;
    
    
    public Voluntario(){
        super();
        this.nomeVoluntario="";
        this.estado= true;
        this.velocidade = 0.0;
        this.raio = 0.0;
        this.codV = "";
        this.certificado = false;
        this.numViagens=1;
        this.classificacao = 0.0;
    }
    
    public Voluntario(String nomeVoluntario,boolean estado,double velocidade, Localizacao localizacao, double raio,String codV,
                    boolean certificado, int numViagens, double classificacao, String user, String pass){
        super(localizacao, user, pass);
        this.nomeVoluntario= nomeVoluntario;
        this.estado= estado;
        this.velocidade = velocidade;
        this.raio = raio;
        this.codV = codV;
        this.certificado = certificado;
        this.numViagens=numViagens;
        this.classificacao = classificacao;
    }
    
    public Voluntario(Voluntario voluntario){
        super(voluntario.getLocalizacao(), voluntario.getUser(), voluntario.getPass());
        this.nomeVoluntario= voluntario.getNomeVoluntario();
        this.estado= voluntario.getEstado();
        this.velocidade = voluntario.getVelocidade();
        this.raio = voluntario.getRaio();
        this.codV = voluntario.getCodV();
        this.certificado = voluntario.getCertificado();
        this.numViagens=voluntario.getNumViagens();
        this.classificacao = voluntario.getClassificacao();
    }
    
    
    public String getNomeVoluntario(){
        return this.nomeVoluntario;
    }
    
    public boolean getEstado(){
        return this.estado;
    }
    
    public double getVelocidade(){
        return this.velocidade;
    }
    

    
    public double getRaio(){
        return this.raio;
    }
    
    public String getCodV(){
        return this.codV;
    }
    
    public boolean getCertificado(){
        return this.certificado;
    }
    
    public int getNumViagens(){
        return this.numViagens;
    }
    
    public double getClassificacao(){
        return this.classificacao;
    }
    
    /** Sets */
    
    public void setNomeVoluntario(String nomeVoluntario){
        this.nomeVoluntario = nomeVoluntario;
    }
    
    public void setEstado(boolean estado){
        this.estado = estado;
    }
    
    public void setVelocidade(double velocidade){
        this.velocidade = velocidade;
    }
    
   
   
    public void setRaio(double raio){
        this.raio = raio;
    }
    
    public void setCodV(String codV){
        this.codV = codV;
    }
    
    public void setCertificado(boolean certificado){
        this.certificado = certificado;
    }
    
    public void setNumViagens(int numViagens){
       this.numViagens = numViagens;
    }
    
    public void setClassificacao(double classificacao){
        this.classificacao = classificacao;
    }
    
    //Método Equals
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Voluntario that = (Voluntario) obj;
        return (super.equals (that) &&
               that.getNomeVoluntario().equals(this.nomeVoluntario) &&
               that.getEstado() == this.estado && 
               that.getVelocidade() == this.velocidade &&
               that.getRaio() == this.raio &&
               that.getCodV() == this.codV &&
               that.getNumViagens() == this.numViagens &&
               that.getClassificacao() == this.classificacao);
    }
    
    //Método clone()
    
    public Voluntario clone(){
        return new Voluntario(this);
    }
    
    
    //Método toString()
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nome do Voluntário: ").append(this.nomeVoluntario);
        sb.append("\nCódigo de Voluntário :  ").append (this.codV);
        sb.append("\nLocalização:  ").append (this.getLocalizacao());
        sb.append("\nUser:  ").append (this.getUser());
        sb.append("\nPass:  ").append (this.getPass());
        sb.append("\nSeu raio :  ").append (this.raio);
        sb.append("\nVelocidade:  ").append (this.velocidade);
        sb.append("\nDisponível: ").append (this.estado);
        sb.append ("\nNúmero de Viagens: ").append(this.numViagens);
        sb.append ("\nClassificação: ").append (this.classificacao);
        
        return sb.toString();
    } 
    
    public boolean aceitoTransporteMedicamentos(){
        return this.certificado;
    }
    
    public void aceitaMedicamentos(boolean state){
        this.certificado = state;
    }

	public void classificar(double cl) {
        this.setClassificacao((numViagens*getClassificacao()+ cl)/(numViagens+1));
        this.setNumViagens(numViagens+1);
	}
}
