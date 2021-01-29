import java.io.Serializable;
import java.util.List;

public class Empresa implements Serializable {
    /** variaveis de instancia */
    private String nome;
    private GPS gps;
    private String email;
    private String password;
    private String nif;

    /** constructores de classe */
    /** vazio */
    public Empresa(){
        this.nome = "";
        this.gps = new GPS();
        this.email = "";
        this.password = "";
        this.nif = "";
    }

    /** parametrico */
    public Empresa(String newNome, double newGpsX, double newGpsY,
                   String newEmail, String newPassword, String newNif){
        this.nome = newNome;
        this.gps = new GPS(newGpsX, newGpsY);
        this.email = newEmail;
        this.password = newPassword;
        this.nif = newNif;
    }

    /** copia */
    public Empresa(Empresa newEmpresa){
        this.nome = newEmpresa.getNome();
        this.gps = new GPS(newEmpresa.getGps());
        this.email = newEmpresa.getEmail();
        this.password = newEmpresa.getPassword();
        this.nif = newEmpresa.getNif();
    }

    /** gets/sets das variaveis de instancia */
    public String getNome(){ return nome; }
    public void setNome(String newNome){ this.nome = newNome; }

    public GPS getGps(){ return this.gps; }
    public void setGps(GPS newGps){ this.gps = newGps; }

    public String getEmail() { return this.email; }
    public void setEmail(String newEmail) { this.email = newEmail; }

    public String getPassword() { return this.password; }
    public void setPassword(String newPassword) { this.password = newPassword; }

    public String getNif(){ return this.nif; }
    public void setNif(String newNif){ this.nif = newNif; }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Empresa passed = (Empresa) o;
        return (this.nome.equals(passed.nome) &&
                this.gps.equals(passed.gps) &&
                this.email.equals(passed.getEmail()) &&
                this.password.equals(passed.getPassword()) &&
                this.nif.equals(passed.nif));
    }

    public String toString(){
        return this.nome + "," + this.gps.toString() + "," + this.email +
                "," + this.password + "," + this.nif;
    }

    public Empresa clone(){
        return new Empresa(this);
    }

    /** metodos especificos */

    /**
     * devolve o valor faturado durante o periodo indicado.
     * NOTA: as datas a passar estao no formato (dd/mm/aa)
     */
    //float facturacao(date inicio, date fim){};

}
