package Model;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import Exceptions.OpcaoInvalidaException;

/**
 * Classe que representa todos os utilizadores 
 */

public class User implements Serializable{
    private static final long serialVersionUID = -6896735489943952618L;
    private String email;
    private String password;
    private String codUtilizador;
    private String nome; 
    private List<Encomendas> encFeitas;
    private double latitude;
    private double longitude;

    public User(){
        this.email="";
        this.password="";
        this.codUtilizador="";
        this.nome="";
        this.encFeitas = new ArrayList<Encomendas>();
        this.latitude=0.0;
        this.longitude=0.0;

    }

    public User(String codU , String nome, String email, String passwd,double latitude, double longitude) {
        this.codUtilizador=codU;
        this.nome=nome;
        this.email = email;
        this.password = passwd;
        this.encFeitas = new ArrayList<>();
        this.latitude=latitude;
        this.longitude=longitude;
    }

    public User(User u) {
        this.email = u.getEmail();
        this.password = u.getPassword();
        this.nome=u.getNome();
        this.codUtilizador=u.getCodUtilizador();
        setEncFeitas(u.getEncFeitas());
        this.latitude=u.getLatitude();
        this.longitude=u.getLongitude();
    }

    /**
    * Método que retorna o email
    */

    public String getEmail() {
        return this.email;
    }

    /**
    * Método que retorna a password
    */

    public String getPassword(){return this.password;}

    /**
    * Método que retorna o nome
    */

    public String getNome() { return nome; }

    /**
    * Método que retorna o código de utilizador
    */
    
    public String getCodUtilizador() { return codUtilizador; }

    /**
    * Método que retorna a lista de encomendas feitas 
    */

    public List<Encomendas> getEncFeitas() {
        ArrayList<Encomendas> e= new ArrayList<>();
        for(Encomendas ec : this.encFeitas)
            e.add(ec.clone());
        return e;
    }

    /**
    * Método que retorna a latitude
    */

    public double getLatitude() { return latitude; }

    /**
    * Método que retorna a longitude
    */

    public double getLongitude() { return longitude; }

    /**
    * Setter do email do utilizador
    */

    public void setEmail(String email) { this.email = email; }

    /**
    * Setter da password do utilizador
    */

    public void setPassword(String password) { this.password = password; }

    /**
    * Setter do nome do utilizador
    */

    public void setNome(String nome){this.nome=nome;}

    /**
    * Setter do codigo do utilizador
    */

    public void setCodUtilizador(String codUtilizador) { this.codUtilizador = codUtilizador; }

    /**
    * Setter das encomendas feitas do utilizador
    */

    public void setEncFeitas(List<Encomendas> encF) {
        this.encFeitas=new ArrayList<>();
        for(Encomendas e : encF)
            this.encFeitas.add(e);
    }

    /**
    * Setter da latitude do utilizador
    */

    public void setLatitude(double latitude) { this.latitude = latitude; }

    /**
    * Setter da longitude do utilizador
    */
    
    public void setLongitude(double longitude) { this.longitude = longitude; }

    /**
    * Método que retorna a lista de encomendas dado um determinado espaço de tempo
    */

    List<Encomendas> getListencUDH(LocalDateTime init, LocalDateTime end) {
        return this.encFeitas.stream()
                .filter(e -> e.getDataHora().isBefore(end) && e.getDataHora().isAfter(init))
                .collect(Collectors.toList());
    }

    /**
    * Método que adiciona uma encomenda ao historico
    */

    public void addEncHist(Encomendas enc) {
        this.encFeitas.add(enc);
    }

    /**
    * Método em que um utilizador classifica o voluntario  
    */

    public void rate(Voluntarios v,int rating) {
        v.rate(rating);
    }

    /**
    * Método em que um utilizador classifica a empresa transportadora   
    */

    public void rate(EmpresasTrans e,int rating){
        e.rate(rating);
    }

    /**
    * Método em que o utilizador pode aceitar ou não a empresa transportadora  
    */

    public boolean aceitarEmpresaTr(double c, double t, EmpresasTrans emp) throws OpcaoInvalidaException {
        System.out.println("Nome da Empresa:" + emp.getNome() +"\nCusto : "+ c + "\nTempo estimado para a entrega : "+ t + "\nAceita (1-sim/ 2-nao): ");
        Scanner sc = new Scanner (System.in);
        int aceita = sc.nextInt();
        if(aceita == 1) return true;
        else if(aceita ==2)  return false;
             else throw new OpcaoInvalidaException();
    }

    /**
    * Método que retorna o utilizador dado um codigo de utilizador
    */

    public User getUser(String codU){
        if(codU.equals(this.nome))
            return this;
        else return null;
    }
    

    public User clone(){return new User(this);}

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return this.email.equals(user.email) &&
                this.password.equals(user.password) &&
                this.nome.equals(user.nome) &&
                this.codUtilizador.equals(user.codUtilizador) &&
                this.encFeitas.equals(user.encFeitas);

    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador:").append(this.email).append(",")
                                     .append(this.password).append(",")
                                     .append(this.codUtilizador).append(",")
                                     .append(this.nome).append(",")
                                     .append(this.encFeitas).append(",")
                                     .append(this.latitude).append(",")
                                     .append(this.longitude).append(",");
        return sb.toString();
    }

}
