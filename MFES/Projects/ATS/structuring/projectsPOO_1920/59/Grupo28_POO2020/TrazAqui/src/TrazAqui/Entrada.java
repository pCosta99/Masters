package TrazAqui;

/**
 * Classe que armazena a informação do utilizador que está a usar o sistema
 */
public interface Entrada {
    String getCod();
    void setCod(String  n);
    String getNome();
    void setNome(String n);
    GPS getLocalizacao();
    void setLocalizacao(GPS a);
    Entrada clone();
    String toString();
    boolean equals(Object o);
    String toStringNome();
    default Entrada newEntrada(String tipo) {
        Entrada a = null;
        switch (tipo) {
            case "Utilizador":
                a = new Utilizador();
                break;
            case "Transportadora":
                a = new Transportadora();
                break;
            case "Voluntario":
                a = new Voluntario();
                break;
            case "Loja":
                a = new Loja();
                break;
            case "LojaFilaEspera":
                a = new LojaFilaEspera();
                break;
            default:
                break;
        }
        return a;
    }
}
