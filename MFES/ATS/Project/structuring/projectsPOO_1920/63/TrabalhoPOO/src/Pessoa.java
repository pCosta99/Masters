import java.io.Serializable;
import java.util.List;

public class Pessoa implements Serializable {
    /** variaveis de instancia */
    private String nome;
    private GPS gps;
    private String email;
    private String password;


    /** constructores de classe */
    /** vazio */
    public Pessoa(){
        this.nome = "";
        this.gps = new GPS();
        this.email = "";
        this.password = "";
    }

    /** parametrico */
    public Pessoa(String newNome, double newGpsX, double newGpsY,
                  String newEmail, String newPassword){
        this.nome = newNome;
        this.gps = new GPS(newGpsX, newGpsY);
        this.email = newEmail;
        this.password = newPassword;
    }

    /** copia */
    public Pessoa(Pessoa newPessoa){
        this.nome = newPessoa.getNome();
        this.gps = new GPS(newPessoa.getGps());
        this.email = newPessoa.getEmail();
        this.password = newPessoa.getPassword();
    }

    /** gets/sets das variaveis de instancia */
    public String getNome(){ return this.nome; }
    public void setNome(String newNome){ this.nome = newNome; }

    public GPS getGps(){ return this.gps; }
    public void setGps(GPS newGps){ this.gps = newGps; }

    public String getEmail() { return this.email; }
    public void setEmail(String newEmail) { this.email = email; }

    public String getPassword() { return this.password; }
    public void setPassword(String newPassword) { this.password = newPassword; }


    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Pessoa passed = (Pessoa) o;
        return ( this.nome.equals(passed.nome) &&
                 this.gps.equals(passed.gps) &&
                this.email.equals(passed.getEmail()) &&
                this.password.equals(passed.getPassword()));
    }

    public String toString(){
        return this.nome + "," + this.gps.toString() + "," +
                this.email + "," + this.password;
    }

    public Pessoa clone(){
        return new Pessoa(this);
    }

    /** metodos especificos */

}