package Projeto;

public class Utilizador {
    private String email;
    private String name;
    private String password;
    private Posicao pos;

    public Utilizador(Posicao pos) {
        this.pos = pos;
    }

    public Utilizador(String email, String name, String pass, Posicao pos){
        this.email = email;
        this.name = name;
        this. password = pass;
        this.pos = pos;
    }

    public Utilizador(Utilizador user) {
        this.email = user.getEmail();
        this.name = user.getNome();
        this.password = user.getPassword();
        this.pos = user.getPos();
    }

    public void definePassword(){
        if (password == null) this.password = "admin";
    }

    public String getEmail() {return this.email;}

    public String getNome(){
        return this.name;
    }

    public String getPassword(){
        return this.password;
    }

    public Posicao getPos(){
        return this.pos;
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }

    @Override
    public String toString() {
        return "Utilizador{" +
                "email='" + email + '\'' +
                ", name='" + name + '\'' +
                ", password='" + password + '\'' +
                ", pos=" + pos +
                '}';
    }
}
