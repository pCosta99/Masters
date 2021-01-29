import java.util.ArrayList;
import java.io.Serializable;

public abstract class Utilizador extends GPS implements Serializable{
    private String nome;
    private String email;
    private String password;
    private GPS localizacao;

    public Utilizador() {
        this.nome = "n/a";
        this.localizacao = new GPS();
        this.email = "n/a";
        this.password = "n/a";
    }

    public Utilizador (String nome, GPS localizacao, String email, String password){
        this.nome = nome;
        this.localizacao = localizacao;
        this.email = email ;
        this.password = password;
    }

    public Utilizador (Utilizador u){
        this.nome = u.getNome();
        this.localizacao = u.getLocalizacao();
        this.email = u.getEmail() ;
        this.password = u.getPassword();
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
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

    public GPS getLocalizacao() {
        return localizacao;
    }

    public void setLocalizacao(GPS localizacao) {
        this.localizacao = localizacao;
    }

    public boolean Login(Utilizador u){
        return this.getPassword().equals(u.getPassword()) && this.getEmail().equals(u.getEmail());
    }

    public boolean equals(Object o){
        if(this == o)
            return true;
        if((o == null) || (this.getClass() != o.getClass()))
            return false;
        Utilizador u = (Utilizador) o;
        return  this.nome.equals(u.getNome()) &&
                this.email.equals(u.getEmail()) &&
                this.password.equals(u.getPassword()) &&
                this.localizacao == u.getLocalizacao();
    }

    public abstract Utilizador clone();
}
