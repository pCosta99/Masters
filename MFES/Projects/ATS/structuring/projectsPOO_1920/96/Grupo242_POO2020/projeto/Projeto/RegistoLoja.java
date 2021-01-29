package Projeto;

public class RegistoLoja extends RegistoEntidade{
    private int codigo;

    public RegistoLoja(int codigo) {
        this.codigo = codigo;
    }

    public RegistoLoja(String nome, Posicao pos, int codigo) {
        super(nome, pos);
        this.codigo = codigo;
    }

    public int getCodigo() {
        return codigo;
    }
}
