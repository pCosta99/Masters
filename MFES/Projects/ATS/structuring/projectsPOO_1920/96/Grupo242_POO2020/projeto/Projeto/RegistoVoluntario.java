package Projeto;

public class RegistoVoluntario extends RegistoPessoa {
    private String password;
    private double raio;

    public RegistoVoluntario(String email, String nome, String pass, Posicao pos, double raio) {
        super(email, nome, pos);
        this.password = pass;
        this.raio = 0;
    }

    public String getPassword() {
        return password;
    }

    public double getRaio() {
        return this.raio;
    }
}