import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

public class Utilizador extends Pessoa implements Comparable<Utilizador>, Serializable {
    /** variaveis de instancia */
    private String codUtilizador;

    /** variaveis de classe */
    private static int totalUtilizadores = 0; //necessario para ir incrementando o codigo do utilizador

    /** constructores de classe */
    /** vazio */
    public Utilizador(){
        super();
        this.codUtilizador = "";
    }

    /** parametrico */
    public Utilizador(String newCodUtilizador, String newNome, double newGpsX, double newGpsY,
                      String newEmail, String newPassword){
        super(newNome, newGpsX, newGpsY, newEmail, newPassword);
        this.codUtilizador = newCodUtilizador;
    }

    public Utilizador(String newCodUtilizador){
        super();
        this.codUtilizador = newCodUtilizador;
    }

    //unico construtor que dá codUtilizador
    public Utilizador(String email, String password){
        super();
        this.setEmail(email);
        this.setPassword(password);
        Utilizador.totalUtilizadores++;
        this.codUtilizador = "u" + Utilizador.totalUtilizadores;
    }

    /** copia */
    public Utilizador(Utilizador newUtilizador){
        super(newUtilizador.getNome(), newUtilizador.getGps().getX(),
                newUtilizador.getGps().getY(), newUtilizador.getEmail(),
                newUtilizador.getPassword());
        this.codUtilizador = newUtilizador.getCodUtilizador();
    }

    /** gets/sets das variaveis de instancia */
    public String getCodUtilizador(){ return this.codUtilizador; }
    public void setCodUtilizador(String newCodUtilizador){
        this.codUtilizador = newCodUtilizador;
    }

    /** metodos override */
    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Utilizador passed = (Utilizador) o;
        return (super.equals(passed) &&
                this.codUtilizador.equals(passed.codUtilizador));
    }

    @Override
    public String toString(){
        return "Utilizador:" + this.codUtilizador + "," + super.toString();
    }

    @Override
    public Utilizador clone(){ return new Utilizador(this); }

    @Override
    public int compareTo(Utilizador that) {
        return this.codUtilizador.compareTo(that.getCodUtilizador());
    }

    /** metodos especificos */

    /**
     * aceitar, ou não, o serviço de entrega proposto por uma empresa transportadora (os
     * serviços feitos por voluntários são automaticamente aceites pelo sistema)
     */
    //public Transportadora selecionaTransportadora(){ };



    /**
     * classificar o voluntário ou a empresa de transportes mediante o grau de satisfação com
     * o serviço
     */
    //void atribuiClassificacao(RegistoEncomenda registoEncomenda){};

}
