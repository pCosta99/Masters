import Exception.*;

import java.io.Serializable;
import java.util.List;
import java.util.ArrayList;

/**
 * Classe que lida com a informação de uma Encomenda.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Encomenda implements Serializable {
    // Instance Variables
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<LinhaEncomenda> linhas;
    private boolean medica;

    // Constructors

    /**
     * Construtor de uma Encomenda.
     */
    public Encomenda() {
        this.codEncomenda  = "n/d";
        this.codUtilizador = "n/d";
        this.codLoja       = "n/d";
        this.peso          = 0.0;
        this.linhas        = new ArrayList<>();
        this.medica        = false;
    }

    /**
     * Construtor de uma Encomenda.
     * @param codigo da Encomenda a construir.
     */
    public Encomenda(String codigo) {
        this.codEncomenda  = codigo;
        this.codUtilizador = "n/d";
        this.codLoja       = "n/d";
        this.peso          = 0.0;
        this.linhas        = new ArrayList<>();
        this.medica        = false;
    }

    /**
     * Construtor de uma Encomenda.
     * @param codEncomenda Código da Encomenda a construir.
     * @param codUtilizador Código do utilizador que fez a Encomenda.
     * @param codLoja Código da Loja onde está a Encomenda.
     * @param peso Peso da Encomenda.
     * @param linhas LinhasEncomenda.
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso,
            List<LinhaEncomenda> linhas) {
        this.codEncomenda  = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja       = codLoja;
        this.peso          = peso;
        this.setLinhas(linhas);
        this.medica        = false;
    }

    /**
     * Construtor de uma Encomenda.
     * @param codEncomenda Código da Encomenda a construir.
     * @param codUtilizador Código do utilizador que fez a Encomenda.
     * @param codLoja Código da Loja onde está a Encomenda.
     * @param peso Peso da Encomenda.
     * @param linhas LinhasEncomenda.
     * @param medica Diz se a Encomenda é médica ou não.
     * @throws EncomendaInvalidaException Quando a Encomenda for inválida.
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso,
                     List<LinhaEncomenda> linhas, boolean medica) throws EncomendaInvalidaException{
        if(peso <= 0 ||
           linhas.stream().anyMatch(l -> l.getQuant() <= 0 || l.getValorUnit() < 0)) throw new EncomendaInvalidaException();
        this.codEncomenda  = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja       = codLoja;
        this.peso          = peso;
        this.setLinhas(linhas);
        this.medica        = medica;
    }

    /**
     * Construtor de uma Encomenda por cópia.
     * @param o Encomenda a copiar.
     */
    public Encomenda(Encomenda o) {
        this.codEncomenda  = o.getCodEncomenda();
        this.codUtilizador = o.getCodUtilizador();
        this.codLoja       = o.getCodLoja();
        this.peso          = o.getPeso();
        this.linhas        = o.getLinhas();
        this.medica        = o.isMedica();
    }

    // Gets

    /**
     * Função que retorna o código da Encomenda.
     * @return Código da Encomenda.
     */
    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    /**
     * Função que retorna o código do Utilizador.
     * @return Código do Utilizador.
     */
    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    /**
     * Função que retorna o código da Loja.
     * @return Código da Loja.
     */
    public String getCodLoja() {
        return this.codLoja;
    }

    /**
     * Função que retorna o Peso da Encomenda.
     * @return Peso da Encomenda.
     */
    public double getPeso() {
        return this.peso;
    }

    /**
     * Função que retorna as Linhas de Encomenda.
     * @return Linhas de Encomenda.
     */
    public List<LinhaEncomenda> getLinhas() {
        List<LinhaEncomenda> ret = new ArrayList<>();
        for (LinhaEncomenda o : this.linhas)
            ret.add(o.clone());
        return ret;
    }

    /**
     * Função que diz se uma Encomenda é médica.
     * @return true se a Encomenda for médica.
     */
    public boolean isMedica(){
        return this.medica;
    }

    // Sets

    /**
     * Função que altera as LinhasEncomenda da Encomenda.
     * @param linhas Novas LinhasEncomenda.
     */
    public void setLinhas(List<LinhaEncomenda> linhas) {
        this.linhas = new ArrayList<>();
        for (LinhaEncomenda o : linhas)
            this.linhas.add(o.clone());
    }

    //

    /**
     * Função que cria um clone de uma Encomenda.
     * @return Encomenda clonada.
     */
    public Encomenda clone() {
        return new Encomenda(this);
    }

    /**
     * Função que compara Encomendas.
     * @param o Objeto a comparar.
     * @return true se forem iguais.
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Encomenda that = (Encomenda) o;
        return that.getCodEncomenda().equals(this.codEncomenda)        &&
               that.getCodUtilizador().equals(this.getCodUtilizador()) &&
               that.getCodLoja().equals(this.codLoja)                  &&
               that.getPeso() == this.peso                             &&
               that.getLinhas().equals(this.linhas)                    &&
               that.isMedica() == this.medica;
    }

    /**
     * Função que converte os parâmetros de uma Encomenda em String.
     * @return String com os parâmetros da Encomenda.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Encomenda: ").append(this.codEncomenda).append("\n");
        sb.append("Codigo Utilizador: ").append(this.codUtilizador).append("\n");
        sb.append("Codigo Loja: ").append(this.codLoja).append("\n");
        sb.append("Peso: ").append(this.peso).append("\n");
        this.linhas.forEach(l -> sb.append("\t").append(l).append("\n"));
        if(this.medica) sb.append("Esta é uma Encomenda Médica!\n");
        return sb.toString();
    }
    //

    /**
     * Cria uma Encomenda a partir de uma linha.
     * @param linha Linha a partir de onde se irá criar uma Encomenda.
     * @return Encomenda criada a partir de uma linha.
     * @throws NumberArgumentsLineException Quando a linha tiver argumentos insuficientes.
     * @throws TypeConvertionException Quando é impossível converter dados.
     */
    static public Encomenda parse(String linha) throws NumberArgumentsLineException, TypeConvertionException {
        String[] campos = linha.split(",");
        if (campos.length < 4) throw new NumberArgumentsLineException();
        double peso;
        List<LinhaEncomenda> ols = new ArrayList<>();
        try{
            peso = Double.parseDouble(campos[3]);
            for (int i = 4; i < campos.length; i += 4) {
                String codProduto = campos[i];
                String descricao = campos[i + 1];
                double quant = Double.parseDouble(campos[i + 2]);
                double valorUnit = Double.parseDouble(campos[i + 3]);
                ols.add(new LinhaEncomenda(codProduto, descricao, quant, valorUnit));
            }
        } catch(NumberFormatException | NullPointerException e) {
            throw new TypeConvertionException("String","Double");
        } catch(IndexOutOfBoundsException e){
            throw new NumberArgumentsLineException();
        }
        return new Encomenda(campos[0],campos[1],campos[2],peso,ols);
    }
}
