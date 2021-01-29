package Projeto;

public class RegistoPessoa {
    private String email;
    private String name;
    private Posicao pos;

    public RegistoPessoa() {
    }

    public RegistoPessoa(String email, String name, Posicao pos) {
        this.email = email;
        this.name = name;
        this.pos = pos;
    }

    public String getEmail() {
        return email;
    }

    public String getName() {
        return name;
    }


    public Posicao getPos() {
        return pos;
    }

}
