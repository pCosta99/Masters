package Projeto;

public class RegistoUtilizador extends RegistoPessoa{
    private String password;

    public RegistoUtilizador(String email, String name, String password, Posicao pos) {
        super(email, name, pos);
        this.password = password;

    }

    public String getPassword() {
        return password;
    }

}
