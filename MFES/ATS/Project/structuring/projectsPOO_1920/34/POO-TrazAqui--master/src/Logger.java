import java.util.Objects;
import java.io.Serializable;

public class Logger implements Serializable {
    private String email;
    private String password;

    public Logger() {
        this.email = new String();
        this.password = new String();
    }

    public Logger(String email, String password) {
        this.email = email;
        this.password = password;
    }

    public Logger(Logger l) {
        this.email = l.getEmail();
        this.password = l.getPassword();
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public boolean equals(Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Logger a = (Logger) o;
        return this.email.equals(a.getEmail())
                && this.password.equals(a.getPassword());
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Email:").append(this.email)
                .append("\nPassword:").append(this.password);
        return sb.toString();
    }

    public Logger clone(){
        return new Logger(this);
    }

    public int hashCode() {
        return Objects.hash(email);
    }
}
