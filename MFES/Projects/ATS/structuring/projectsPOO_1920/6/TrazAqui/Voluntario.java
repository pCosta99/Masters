
/**
 * Classe dos Voluntarios
 * 
 * @author (Jo�o Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
public class Voluntario implements aceitaEncomendasMedicas, Serializable
{
    // variaveis de instancia da classe Voluntario
    private String codVoluntario;
    private String nomeVoluntario;
    private double coordX;
    private double coordY;
    private double raio;
    private String email;
    private String password;
    private boolean estado;
    private boolean state;
    
    // construtor por omiss�o
    public Voluntario(){
        this.codVoluntario = "n/a";
        this.nomeVoluntario = "n/a";
        this.coordX = 0.0;
        this.coordY = 0.0;
        this.raio = 1.0;
        this.email = "n/a";
        this.password = "n/a";
    }
    
    // construtor parametrizado
    public Voluntario(String cod, String nome, double cX, double cY, double raio){
        this.codVoluntario = cod;
        this.nomeVoluntario = nome;
        this.coordX = cX;
        this.coordY = cY;
        this.raio = raio;
        this.email = cod+"@hotmail.com";
        this.password = cod;
    }
    
    // construtor de cópia
    public Voluntario(Voluntario v){
        this.codVoluntario = v.getCodV();
        this.nomeVoluntario = v.getNomeV();
        this.coordX = v.getCoordX();
        this.coordY = v.getCoordY();
        this.raio = v.getRaio();
        this.email = v.getCodV()+"@hotmail.com";
        this.password = v.getCodV();
    }
    
    //  metodo que devolve o codigo do voluntario (ex: v10101)
    public String getCodV(){
        return this.codVoluntario;
    }
    
    // metodo que devolve o nome do Voluntario
    public String getNomeV(){
        return this.nomeVoluntario;
    }
    
    // metodo que devolve a coordenada X
    public double getCoordX(){
        return this.coordX;
    }
    
    // metodo que devolve a coordenada Y
    public double getCoordY(){
        return this.coordY;
    }
    
    // metodo que devolve o raio
    public double getRaio(){
        return this.raio;
    }
    
    // metodo que devolve o email
    public String getEmail(){
        return this.email;
    }
    
    // metodo que devolve a password
    public String getPassword(){
        return this.password;
    }
    
    // metodo que devolve a disponibilidade de entrega de uma encomenda
    public boolean getEstado(){
        return this.estado;
    }
    
    // metodo para definir o codigo do Voluntario
    public void setCodV(String codV){
        this.codVoluntario = codV;
    }
    
    // metodo para definir o nome do Voluntario
    public void setNome(String nome){
        this.nomeVoluntario = nome;
    }
    
    // metodo para definir a coordenada X
    public void setCoordX(double cX){
        this.coordX = cX;
    }
    
    // metodo para definir a coordenada Y
    public void setCoordY(double cY){
        this.coordY = cY;
    }
    
    // metodo para definir o raio
    public void setRaio(double raio){
        this.raio = raio;
    }
    
    // metodo para definir estado
    public void setEstado(boolean estado){
        this.estado = estado;
    }
    
    // metodo para verificar se pode entregar encomendas medicas
    public boolean aceitoTransporteMedicamentos(){
        if (this.state == true) return true;
        else return false;
    }
    
    // metodo para alterar o state
    public void aceitaMedicamentos(boolean state){
        this.state = state;
    }
    
    // metodo que coloca toda a informação sobre um voluntário numa string
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append("Voluntario:"+this.codVoluntario+","+this.nomeVoluntario+","+this.coordX+","+this.coordY+","+this.raio);
        return sb.toString();
    }
    
    // metodo de copia de um voluntario
    public Voluntario clone(){
        return new Voluntario(this);
    }
    
    // metodo que compara se dois voluntarios sao iguais
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o.getClass()!=this.getClass())||o==null) return false;
        Voluntario v = (Voluntario) o;
        return this.codVoluntario.equals(v.getCodV()) &&
               this.nomeVoluntario.equals(v.getNomeV()) &&
               this.coordX == v.getCoordX() &&
               this.coordY == v.getCoordY() &&
               this.raio == v.getRaio();
    }
}
