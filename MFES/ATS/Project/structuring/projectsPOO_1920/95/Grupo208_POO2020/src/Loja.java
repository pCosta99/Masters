import Exception.*;

import java.io.Serializable;

/**
 * Classe que lida com a informação de uma loja.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Loja extends Entidade implements Serializable {
    private double tempoMedio; //hora
    private int numClientes;

    // Constructors

    /**
     * Construtor de uma loja.
     */
    public Loja() {
        super();
        this.tempoMedio  = 5;
        this.numClientes = 0;
    }

    /**
     * Construtor de uma loja.
     * @param codigo, Código da loja a construir.
     * @param nome, Nome da loja a construir.
     * @param localizacao, Localização da loja a construir.
     */
    public Loja(String codigo, String nome, Localizacao localizacao) {
        super(codigo, nome, localizacao);
        this.tempoMedio = 5;
        this.numClientes = 0;
    }

    /**
     * Construtor de uma loja.
     * @param codigo, Código da loja a construir.
     * @param nome, Nome da loja a construir.
     * @param mail, Mail da loja a construir.
     * @param password, Password da loja a construir.
     * @param localizacao, Localização da loja a construir.
     */
    public Loja(String codigo, String nome, String mail, String password, Localizacao localizacao){
        super(codigo,nome,mail,password,localizacao);
        this.tempoMedio  = 0;
        this.numClientes = 0;
    }

    /**
     * Construtor de uma loja.
     * @param loja, Loja a construir.
     */
    public Loja(Loja loja) {
        super(loja);
        this.tempoMedio  = loja.getTempoMedio();
        this.numClientes = loja.getNumClientes();
    }

    /**
     * Função que retorna o número de clientes.
     * @return Número de clientes.
     */
    private int getNumClientes(){
        return this.numClientes;
    }

    /**
     * Função que retorna o tempo médio.
     * @return Tempo médio
     */
    private double getTempoMedio() {
        return this.tempoMedio;
    }

    /**
     * Função que cria um clone de uma Loja.
     * @return Loja clonada.
     */
    public Loja clone() {
        return new Loja(this);
    }

    /**
     * Função que converte os parametros da loja para String.
     * @return String com os parametros da loja.
     */
    public String toString() {
        return super.toString() + "\nTempo médio: " + this.numClientes;
    }

    /**
     * Função que compara parametros da Loja.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        return super.equals(o);
    }
    //

    /**
     * Função que cria uma Loja a partir de uma string.
     * @param linha, String a partir da qual se irá retirar os campos que permitem a criação de uma loja.
     * @return Loja.
     * @throws NumberArgumentsLineException, Quando o número de argumentos for inválido.
     * @throws TypeConvertionException, Quando o tipo for inválido.
     */
    static public Loja parse(String linha) throws NumberArgumentsLineException, TypeConvertionException {
        String[] campos = linha.split(",");
        if (campos.length != 4)
            throw new NumberArgumentsLineException();
        double lat,lon;
        try{ lat = Double.parseDouble(campos[2]);
            lon = Double.parseDouble(campos[3]);}
        catch(NumberFormatException | NullPointerException e) {
            throw new TypeConvertionException("String","Double");
        }
        return new Loja(campos[0],campos[1],new Localizacao(lat,lon));
    }

    /**
     * Função que incrementa o número de clientes.
     */
    public void incCliente(){
        this.numClientes++;
    }

    /**
     * Função que decrementa o número de clientes.
     */
    public void decCliente(){
        this.numClientes--;
    }

    /**
     * Função que calcula o tempo total.
     * @return, Tempo total.
     */
    public double getTempoTotal(){
        return this.tempoMedio * this.numClientes;
    }

    /**
     * Função que modifica o tempo médio.
     * @param tempoMedio, Novo tempo médio.
     * @throws NotPositiveNumberException, Quando o número nao for positivo.
     */
    public void setTempoMedio(double tempoMedio) throws NotPositiveNumberException{
        if(tempoMedio < 0) throw new NotPositiveNumberException(tempoMedio);
        this.tempoMedio = tempoMedio;
    }
}
