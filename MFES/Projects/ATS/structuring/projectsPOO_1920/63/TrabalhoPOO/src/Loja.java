import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Loja extends Empresa implements Serializable {
    /** variaveis de instancia */
    private String codLoja;
    //double	filaEspera;
    //double	tempoMedioAtendimento;

    /** variaveis de classe */
    private static int totalLojas = 0; //necessario para ir incrementando o codigo da loja

    /** constructores de classe */
    /** vazio */
    public Loja(){
        super();
        this.codLoja = "";
    }

    /** parametrico */
    public Loja(String newCodLoja, String newNome, double newGpsX, double newGpsY,
                String newEmail, String newPassword, String newNif){
        super(newNome, newGpsX, newGpsY, newEmail, newPassword, newNif);
        this.codLoja = newCodLoja;
    }

    public Loja(String newCodLoja){
        super();
        this.codLoja = newCodLoja;
    }

    //unico construtor que dá codLoja
    public Loja(String email, String password){
        super();
        this.setEmail(email);
        this.setPassword(password);
        Loja.totalLojas ++;
        this.codLoja = "l" + Loja.totalLojas;
    }

    /** copia */
    public Loja(Loja newLoja){
        super(newLoja);
        this.codLoja = newLoja.getCodLoja();
    }

    /** gets/sets das variaveis de instancia */
    public String getCodLoja(){ return this.codLoja; }
    public void setCodLoja(String newCodLoja){ this.codLoja = newCodLoja; }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Loja passed = (Loja) o;
        return (super.equals(passed) &&
                this.codLoja.equals(passed.getCodLoja()));
    }

    public String toString(){
        return "Loja:" + this.codLoja + "," + super.toString();
    }

    public Loja clone(){
        return new Loja(this);
    }

    /** metodos especificos */

    /**
     * sinalizar que existe uma encomenda de um utilizador para ser entregue
     */
    //todo

    /**
     * se possível, indicar a quantidade de pessoas em fila para serem atendidas
     */
    //todo

    /**
     * Gerar uma estimativa para o tempo de Espera
     */
    public double tempoDeEspera(){
        Random numeroPessoas = new Random();
        Random tempoPorPessoa = new Random();
        return (double) numeroPessoas.nextInt(5) * tempoPorPessoa.nextInt(4);
    }
}
