package Projeto;

public class RegistoEntidade {
    private String nome;
    private Posicao pos;

    public RegistoEntidade() {
    }

    public RegistoEntidade(String nome, Posicao pos){
        this.nome = nome;
        this.pos = pos;
    }

    public String getNome() {
        return nome;
    }

    public Posicao getPos() {
        return pos;
    }
}
