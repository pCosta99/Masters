package src.model;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que cria a Classificação de uma model.Transportadora ou model.Voluntario
 */

public class Classificacao implements Serializable {

    //Variáveis de Instância
    private Map<String,Double> classPorEntrega;
    private double valorClass;

    /**
     * Construtores da classe model.Classificacao.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Classificacao.
     */

    public Classificacao(){
        this.classPorEntrega = new HashMap<>();
        this.valorClass = 0;
    }

    /**
     * Construtor parametrizado de model.Classificacao.
     * Aceita como parâmetros os valores para cada variável de instância.
     */

    public Classificacao(HashMap<String,Double> classPorEntrega, double valorClass){
        this.setClassPorUtilizador(classPorEntrega);
        this.valorClass = valorClass;
    }

    /**
     * Construtor de cópia de model.Classificacao.
     * Aceita como parâmetro outra model.Classificacao e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Classificacao(Classificacao c){
        this.classPorEntrega = c.getClassPorEntrega();
        this.valorClass = c.getValorClass();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve uma cópia do registo interno das classificações
     * dadas ao "dono" do objeto model.Classificacao.
     * @return Map com toda a informação do registo interno de classificações
     * dadas.
     */

    public Map<String,Double> getClassPorEntrega() {
        Map<String,Double> res = new HashMap<>();

        for(Map.Entry<String,Double> e : this.classPorEntrega.entrySet()){
            res.put(e.getKey(),e.getValue());
        }

        return res;
    }

    /**
     * Devolve o valor da classificação atribuída ao "dono" do objeto model.Classificacao
     * @return double com o valor da classificação global
     */

    public double getValorClass() {
        return this.valorClass;
    }

    /**
     * Atualiza o registo interno de classificações
     * @param classPorUtilizador novo registo de classificações
     */

    public void setClassPorUtilizador(Map<String,Double> classPorUtilizador) {
        Map<String,Double> res = new HashMap<>();
        for(Map.Entry<String,Double> e : classPorUtilizador.entrySet()){
            res.put(e.getKey(),e.getValue());
        }
        this.classPorEntrega = res;
    }

    /**
     * Atualiza o valor da classificação
     * @param valorClass novo valor da classificação
     */

    public void setValorClass(double valorClass) {
        this.valorClass = valorClass;
    }
    
    /**
     * Método que adiciona ou altera uma classificação fornecida por um usuário recalculando o valor de classificação
     * dada ao "dono" de tal classificação
     * @param valor valor da classificação atribuída pelo usuário
     * @return double com o valor da média de classificação recalculada 
     */

    public void addClassificacao(String codEnc, double valor){

        if (!this.classPorEntrega.containsKey(codEnc)) {

            this.classPorEntrega.put(codEnc, valor);
           this.recalculaClassfc();
        }

        //else throw EncomendaJaAvaliadaException
    }

    /**
     * Método que recalcula o valor de classificação obtida, fazendo set do novo valor obtido
     *  @return double com o valor da nova classificação calculada 
     */

    public double recalculaClassfc(){
        double r = 0;
        int n = 0;
        for (double valor: this.classPorEntrega.values()){
            r += valor;
            n++;             
        }

        if (n > 0) {
            r = r/n;
        }

        this.setValorClass(r);
        return r;
    }

    /**
     * Método que transforma um objeto model.Classificacao numa String
     * @return String com toda a informação presente no Objeto
     */

    public String toString() {
        final StringBuilder sb = new StringBuilder("model.Classificacao{");
        sb.append("classPorUtilizador=").append(this.classPorEntrega.toString());
        sb.append(", valorClass=").append(this.valorClass);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método que determina se uma classificação e outro Objeto são iguais
     * @param o Object
     * @return boolean consoante a igualdade da model.Classificacao e o Object recebido como parâmetro
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Classificacao c = (Classificacao) o;
        return c.getValorClass() == this.valorClass  &&
                this.classPorEntrega.equals(c.getClassPorEntrega());
    }

    /**
     * Método que clona uma Classificacão, para tal é usado o construtor de cópia
     * @return Objeto model.Classificacao que é uma cópia da model.Classificacao
     */

    public Classificacao clone(){
        return new Classificacao(this);
    }

}
