import java.util.*;
import java.io.Serializable;

public class Utilizador implements Serializable{
    /*Variaveis de instância*/
    private String codUser;
    private String nome;
    private double gpsX;
    private double gpsY;
    private String email;    
    private String password;
    private List <Encomenda> encomendas; 

    /*Construtores*/
    public Utilizador(){
        codUser = null;
        nome = null;
        gpsX = 0.0;
        gpsY = 0.0;
        email = null;
        password = null;
        encomendas = new ArrayList<>();
    }
    public Utilizador(String codUser, String nome, double gpsX, double gpsY, String email, String password, List<Encomenda> encomendas){
        this.codUser = codUser;
        this.nome = nome;
        this.gpsX = gpsX;
        this.gpsY = gpsY;
        this.email = email;
        this.password = password;
        this.encomendas = encomendas;
    }
    public Utilizador(Utilizador u){
        this.codUser = u.getCodUser();
        this.nome = u.getNome();
        this.gpsX = u.getGPSX();
        this.gpsY = u.getGPSY();
        this.email = u.getEmail();
        this.password = u.getPassword();
        List <Encomenda> encomendasClone = new ArrayList<>();
        for(Encomenda e : u.encomendas){
            encomendasClone.add(e.clone());
        }
        this.encomendas = encomendasClone;
    }

    /*Metodos de instância*/

    public String getCodUser(){
        return codUser;
    }

    public String getNome(){
        return nome;
    }

    public double getGPSX() {
        return gpsX;
    }

    public double getGPSY() {
        return gpsY;
    }

    public String getEmail(){
        return email;
    }
        
    public String getPassword(){
        return password;
    }
    
    public List<Encomenda> getEncomendasEstado(int s){
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda a : this.encomendas)
            if(a.getEstado() == s)
                aux.add(a.clone());
        return aux;
    }
    
    public List<Encomenda> getEncomendas(){
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda a : this.encomendas)
            aux.add(a.clone());
        return aux;
    }
    
    public void setCodUser(String codUser){
        this.codUser = codUser;
    }

    public void setNome(String nome){
        this.nome = nome;
    }

    public void setGPS(double gpsX, double gpsY){
        this.gpsX = gpsX;
        this.gpsY = gpsY;
    }

    public void setGPSX(double gpsX){
        this.gpsX = gpsX;
    }

    public void setGPSY(double gpsY){
        this.gpsY = gpsY;
    }

    public void setEmail(String email){
        this.email = email;
    }
    
    public void setPassword(String password){
        this.password = password;
    }
    
    public void setEncomendas(List<Encomenda> al){
        this.encomendas = new ArrayList<>();
        for(Encomenda a : al)
            this.encomendas.add(a.clone());
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }

    public String toString(){
        StringBuilder aux = new StringBuilder("Email: " + this.email + ";\n"
                + "Nome: " + this.nome + ";\n"
                + "Password: " + this.password + ";\n"
                + "Código: " + this.codUser + ";\n"
                + "Coordenadas: " + this.gpsX + "," + this.gpsY + ";\n"
                + "Encomendas: \n");
        for(Encomenda a : this.encomendas)
            switch(a.getEstado()){
                case 0: aux.append("-").append(a.getCodEnc()).append(": Terminada").append("\n");
                    break;
                case 1: aux.append("-").append(a.getCodEnc()).append(": Espera sinalizacao da loja").append("\n");
                    break;
                case 2: aux.append("-").append(a.getCodEnc()).append(": Espera em loja").append("\n");
                    break;
                case 3: aux.append("-").append(a.getCodEnc()).append(": Espera Resposta do Utilizador(Transportadora)").append("\n");
                    break;
            }
        return aux.toString();
    }

    public boolean equals(Object obj){
        if(this == obj)
            return true;
        else if((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        else{
            Utilizador ft = (Utilizador) obj;
            return codUser.equals(ft.getCodUser()) && nome.equals(ft.getNome())
                    && gpsX == ft.getGPSX() && gpsY == ft.getGPSY() && email.equals(ft.getEmail())
                    && password.equals(ft.getPassword()) && encomendas == ft.getEncomendas();
        }
    }

    
    public int hashCode() {
        final int primo = 31;
        int result = 1;
        result = primo * result + ((codUser == null) ? 0 : codUser.hashCode());
        result = primo * result + ((nome == null) ? 0 : nome.hashCode());
        result = primo * result + (int) gpsX + (int) gpsY;
        result = primo * result + ((email == null) ? 0 : email.hashCode());
        result = primo * result + ((password == null) ? 0 : password.hashCode());
        for(Encomenda e : this.encomendas)
            result = primo * result + e.hashCode();
        return result;
    }
}
