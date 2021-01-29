package Projeto;

public class Cliente extends Utilizador {

    public Cliente(String email, String nome, String pass, Posicao pos) {
        super(email, nome, pass, pos);
    }

    public Cliente(Cliente cli){
        super(cli);
    }

    public Cliente clone() {
        return new Cliente(this);
    }
}
