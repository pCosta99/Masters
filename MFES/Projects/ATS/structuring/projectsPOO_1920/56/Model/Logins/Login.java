package Model.Logins;


import java.io.Serializable;
import java.util.Objects;

public class Login implements ILogin, Serializable {
    String email;
    String password;

    public Login(){
        this.email = "InserirCódigo@trazAqui.com";
        this.password = "Código_0000";
    }
    public Login(String email,String password){
        this.email = email;
        this.password = password;
    }
    public Login (Login l){
        this.email = l.getEmail();
        this.password = l.getPassword();
    }

    //GETTERS
    public String getEmail() {
        return this.email;
    }
    public String getPassword() {
        return this.password;
    }

    //SETTERS
    public void setEmail(String email) {
        this.email = email;
    }
    public void setPassword(String password) {
        this.password = password;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Login)) return false;
        Login login = (Login) o;
        return Objects.equals(getEmail(), login.getEmail()) &&
                Objects.equals(getPassword(), login.getPassword());
    }

    public int hashCode() {
        return Objects.hash(getEmail(), getPassword());
    }

    public String toString() {
        return email + ", " +
                password + "\n" ;
    }

    public void setLogin(String id){
        String pass;
        String email = id + "@trazAqui.com"; // o email é "u??.user@trazAqui.com" u ou l ou t ou v
        /*Random rand = new Random();
        int rand_int = rand.nextInt(100);*/
        pass = id + "_" + "0000" ; // a password é "u??_0000"
        this.email = email;
        this.password = pass;
    }
}
