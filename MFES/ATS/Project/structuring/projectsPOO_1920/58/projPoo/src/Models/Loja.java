package Models;

import java.io.Serializable;
import java.util.Random;

public class Loja implements Serializable {
    private String id;
    private String nome;
    private GPS g;
    private String email;
    private String password;
    private int tempoAtendimento;
    private int filaDeEspera;

    /**
     * Construtor por omissão.
     */
    public Loja(){
        this.id = "";
        this.nome = "";
        this.g = new GPS();
        this.email = "";
        this.password = "";
        Random r = new Random();
        this.tempoAtendimento = r.nextInt(10);
        this.filaDeEspera = 0;
    }

    /**
     * Construtor por omissão.
     * @param id String que representa o código de uma Loja.
     * @param nome String que representa o nome de uma Loja.
     * @param g Objeto da classe GPS que representa as coordenadas.
     * @param email String que representa o email.
     * @param password String que representa a password.
     */
    public Loja(String id, String nome,GPS g, String email, String password, int tempoAtendimento, int filaDeEspera){
        this.id = id;
        this.nome = nome;
        this.g = new GPS(g);
        this.email = email;
        this.password = password;
        this.filaDeEspera = filaDeEspera;
        this.tempoAtendimento = tempoAtendimento;
    }

    /**
     * Construtor por cópia.
     * @param l Recebe um objeto da classe Loja.
     */
    public Loja(Loja l){
        this.id = l.getL();
        this.nome = l.getNome();
        this.g = new GPS(l.getGps());
        this.email = l.getEmail();
        this.password = l.getPassword();
        this.tempoAtendimento = l.getTempoAtendimento();
        this.filaDeEspera = l.getFilaDeEspera();
    }

    /**
     * Função que retorna o id (username) da loja.
     * @return - Id da loja.
     */
    public String getId() {
        return id;
    }

    /**
     * Função que retorna uma estimativa do tempo de atendimento de uma encomenda/pedido.
     * @return - Tempo de atendimento.
     */
    public int getTempoAtendimento(){
        return this.tempoAtendimento;
    }

    /**
     * Função que retorna o número de pessoas na fila de espera da loja.
     * @return - Número de pessoas na fila de espera.
     */
    public int getFilaDeEspera(){
        return this.filaDeEspera;
    }

    /**
     * Método que dá o código de uma Loja.
     * @return Devolve uma String do código.
     */
    public String getL(){
        return this.id;
    }

    /**
     * Método que define o código de uma Loja.
     * @param id Recebe uma String do código.
     */
    public void setL(String id){
        this.id = id;
    }

    /**
     * Método que dá o nome de uma Loja.
     * @return Devolve uma String do nome.
     */
    public String getNome(){
        return this.nome;
    }

    /**
     * Método que define o nome de uma Loja
     * @param n Recebe uma String do nome.
     */
    public void setNome(String n){
        this.nome = n;
    }

    /**
     * Método que dá o GPS.
     * @return  Devolve o GPS.
     */
    public GPS getGps(){
        return this.g;
    }

    /**
     * Método que define um GPS.
     * @param g Recebe um objeto da classe GPS.
     */
    public void setGps(GPS g){
        this.g = g;
    }

    /**
     * Método que dá o email.
     * @return Devolve o email.
     */
    public String getEmail() {
        return this.email;
    }

    /**
     * Método que define um email.
     * @param email Recebe um email.
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Método que dá uma password.
     * @return Devolve uma password.
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Método que define uma password.
     * @param password Recebe uma password.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Função que verifica se o objeto recebido é idêntico ao da classe Loja.
     * @param o Recebe um objeto.
     * @return Devolve um boolean com a respetiva verificação.
     */
    @Override
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        Loja l = (Loja) o;
        return l.getL().equals(this.id) &&
                l.getNome().equals(this.nome) &&
                l.getGps().equals(this.g) &&
                l.getEmail().equals(this.email) &&
                l.getPassword().equals(this.password);
    }

    /**
     * Função que traduz a classe Loja.
     * @return Devolve uma String com a respetiva tradução.
     */
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Loja:  ").append(this.id)
                .append("\nNome da Loja:  ").append(this.nome)
                .append("\nGPS:  ").append(this.g)
                .append("\nEmail:  ").append(this.email)
                .append("\nPassword:  ").append(this.password)
                .append("\nAtendimento: ").append(this.tempoAtendimento).append(" min")
                .append("\nFila de Espera: ").append(this.filaDeEspera).append(" pessoas");
        return sb.toString();
    }

    public String toString2(){
        StringBuilder sb = new StringBuilder();
        sb.append("Loja:  ").append(this.id)
                .append("\nNome da Loja:  ").append(this.nome);

        return sb.toString();
    }

    /**
     * Função que faz um clone da classe Loja.
     * @return Devolve esse clone.
     */
    @Override
    public Loja clone(){
        return new Loja(this);
    }

    /**
     * Função que calcula o tempo que a loja demorar a ter uma encomenda pronta.
     * @return - Tempo que a encomenda demora a ficar pronta.
     */
    public int tempoDoPedido(){
        int ret = 0;
        if(this.filaDeEspera == 0) ret = this.tempoAtendimento;
        else ret = this.filaDeEspera * this.tempoAtendimento;
        return ret;
    }

    public void setFilaDeEspera(int g){
        this.filaDeEspera = g;
    }
}
