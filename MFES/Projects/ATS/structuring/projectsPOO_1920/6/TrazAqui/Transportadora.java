
/**
 * Classe das Transportadoras
 * 
 * @author (João Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.time.LocalDateTime;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
public class Transportadora implements aceitaEncomendasMedicas, Serializable
{
    // variaveis de instancia da classe Transportadora
    private String codEmpresa;
    private String nomeEmpresa;
    private double coordX;
    private double coordY;
    private long nif;
    private double raio;
    private double precoKm;
    private String email;
    private String password;
    private boolean estado;
    private boolean state;
    
    // construtor por omissão
    public Transportadora(){
        this.codEmpresa = "n/a";
        this.nomeEmpresa = "n/a";
        this.coordX = 0.0;
        this.coordY = 0.0;
        this.nif = 0;
        this.raio = 0.0;
        this.email = "n/a";
        this.password = "n/a";
    }
    
    // construtor parametrizado
    public Transportadora(String codE, String nomeE, double cX, double cY,
                          long nif, double raio, double preco){
        this.codEmpresa = codE;
        this.nomeEmpresa = nomeE;
        this.coordX = cX;
        this.coordY = cY;
        this.nif = nif;
        this.raio = raio;
        this.precoKm = preco;
        this.email = codE+"@empresa.com";
        this.password = codE;
    }
    
    // construtor de copia
    public Transportadora(Transportadora t){
        this.codEmpresa = t.getCodE();
        this.nomeEmpresa = t.getNomeE();
        this.coordX = t.getCoordX();
        this.coordY = t.getCoordY();
        this.nif = t.getNif();
        this.raio = t.getRaio();
        this.precoKm = t.getPrecoKm();
        this.email = t.getCodE()+"@empresa.com";
        this.password = t.getCodE();
    }
    
    // metodo que devolve o codigo da empresa
    public String getCodE(){
        return this.codEmpresa;
    }
    
    // metodo que devolve o nome da empresa
    public String getNomeE(){
        return this.nomeEmpresa;
    }
    
    // metodo que devolve a coordenada X
    public double getCoordX(){
        return this.coordX;
    }
    
    // metodo que devolve a coordenada Y
    public double getCoordY(){
        return this.coordY;
    }
    
    // metodo que devolve o nif da empresa
    public long getNif(){
        return this.nif;
    }
    
    // metodo que devolve o raio em que a empresa opera
    public double getRaio(){
        return this.raio;
    }
    
    // metodo que devolve o preco por km de uma empresa
    public double getPrecoKm(){
        return this.precoKm;
    }
    
    //metodo que devolve o email
    public String getEmail(){
        return this.email;
    }
    
    // metodo que devolve a password
    public String getPassword(){
        return this.password;
    }
    
    //metodo que devolve a disponibilidade de entrega de uma encomenda
    public boolean getEstado(){
        return this.estado;
    }
    
    //metodo que devolve o estado em relacao a poder entregar encomendas medicas
    public boolean getState(){
        return this.state;
    }
    
    //metodo para definir o codigo da empresa
    public void setCodE(String cod){
        this.codEmpresa = cod;
    }
    
    // metodo para definir o nome da empresa
    public void setNomeE(String nome){
        this.nomeEmpresa = nome;
    }
    
    // metodo para definir a coordenada X
    public void setCoordX(double cX){
        this.coordX = cX;
    }
    
    // metodo para definir a coordenada Y
    public void setCoordY(double cY){
        this.coordY = cY;
    }
    
    // metodo para definir o nif
    public void setNif(long nif){
        this.nif = nif;
    }
    
    // metodo para definir o raio
    public void setRaio(double raio){
        this.raio = raio;
    }
    
    // metodo para definir o preco por km
    public void setPrecoKm(double preco){
        this.precoKm = preco;
    }
    
    //metodo para definir se esta disponivel para fazer entrega de encomendas
    public void setEstado(boolean estado){
        this.estado = estado;
    }
    
    // metodo para verificar se pode entregar encomendas medicas
    public boolean aceitoTransporteMedicamentos(){
        if (state == true) return true;
        else return false;
    }
    
    // metodo para alterar o state
    public void aceitaMedicamentos(boolean state){
        this.state = state;
    }
    
    // metodo que coloca toda a informacao sobre uma transportadora numa string
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append("Transportadora:"+this.codEmpresa+","+this.nomeEmpresa+","+this.coordX+","+this.coordY+","+
                  this.nif+","+this.raio+","+this.precoKm);
        return sb.toString();
    }
    
    // metodo de copia de uma transportadora
    public Transportadora clone(){
        return new Transportadora(this);
    }
    
    // metodo que compara se duas transportadoras sao iguais
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o.getClass()!=this.getClass())||o==null) return false;
        Transportadora t = (Transportadora) o;
        return this.codEmpresa.equals(t.getCodE()) &&
               this.nomeEmpresa.equals(t.getNomeE()) &&
               this.coordX == t.getCoordX() &&
               this.coordY == t.getCoordY() &&
               this.nif == t.getNif() &&
               this.raio == t.getRaio() &&
               this.precoKm == t.getPrecoKm();
        }
}
