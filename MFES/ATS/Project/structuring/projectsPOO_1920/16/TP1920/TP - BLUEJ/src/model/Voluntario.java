package src.model;

import java.util.List;
import java.util.Objects;

/**
 * Classe que cria Voluntários
 */

public class Voluntario extends Distribuidor{

    //Variáveis de Classe
    private static String codVolGlobal = "v0";

    //Variáveis de instância
    private Entrega entregaAtiva;

    /**
     * Devolve o username de Voluntairo gerado para o próximo voluntario a ser criado
     * @return String com o novo username de voluntario
     */
    private static String getCodVolGlobal(){
        String cod = Voluntario.codVolGlobal;
        String inc = cod.substring(1);
        int i = Integer.parseInt(inc);
        i = i+1;
        Voluntario.setCodVolGlobal("v"+i);
        return cod;
    }


    /**
     * Atualiza o username de Voluntairo gerado para o próximo voluntario a ser criado
     * @param String com o novo username de voluntario
     */
    private static void setCodVolGlobal(String cod){
        Voluntario.codVolGlobal = cod;
    }

    /**
     * Atualiza o username de Voluntario a ser atribuido para o Voluntario seguinte, baseado no ultimo codigo atribuído através de logs;
     * @param cod String com o ultimo username de Voluntario usado nos logs
     */
    public static void updateCodGlobal(String cod){
        String atual = Voluntario.getCodVolGlobal();
        int numberAtual = Integer.parseInt(atual.substring(1));
        int numberCod = Integer.parseInt(cod.substring(1));
        if (numberCod > numberAtual) {
            Voluntario.setCodVolGlobal("v"+(numberCod+1));
        }
    }

    /**
     * Construtores da classe model.Voluntario.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Voluntario.
     */

    public Voluntario(){
        super();
        this.setUsername(Voluntario.getCodVolGlobal());
        this.entregaAtiva = new Entrega();
    }

    /**
     * Construtor parametrizado de model.Voluntario.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */

    public Voluntario(String password, String nome, String email, double raio, Classificacao classificacao, boolean recolhe, Ponto localizacao, List<Entrega> historicoEntregas){
        super(Voluntario.getCodVolGlobal(),password, nome,email,recolhe,historicoEntregas,classificacao,raio,localizacao);
        this.entregaAtiva = new Entrega();
    }

    /**
     * Construtor de cópia de model.Voluntario.
     * Aceita como parâmetro outro model.Voluntario e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Voluntario(Voluntario v){
        super(v.getUsername(),v.getPassword(),v.getNome(),v.getEmail(),v.estaARecolher(),v.getHistoricoEntregas(),v.getClassificacao(),v.getRaio(),v.getLocalizacao());
        this.entregaAtiva = v.getEntregaAtiva();
    }


    /**
     * Construtor parametrizado de model.Voluntario para criação de instância a partir de logs.
     * Aceita como parâmetros os valores para cada variável de instância.
     */
    public Voluntario(String cod, String password, String nome, String email, double raio, Classificacao classificacao, boolean recolhe, Ponto localizacao, List<Entrega> historicoEntregas){
        super(cod,password, nome,email,recolhe,historicoEntregas,classificacao,raio,localizacao);
        Voluntario.updateCodGlobal(cod);
        this.entregaAtiva = new Entrega();
    }


    /**
     * Métodos de Instância
     */

    /**
     * Atualiza o valor da entrega ativa do model.Voluntario
     * @param e Entrega a ser colocada como ativa no Voluntario
    */
    public void addEntregaAtiva(Entrega e){
        this.entregaAtiva = e.clone();
    }

    /**
     * Devolve o valor da entrega ativa do model.Voluntario
     * @return model.Entrega Entrega ativa do Voluntario
    */
    public Entrega getEntregaAtiva(){
        return this.entregaAtiva.clone();
    }

    /**
     * Remove a atual encomenda ativa do model.Voluntario
    */

    public void removeEntregaAtiva(){
        this.entregaAtiva = null;
    }


    /**
     * Método que transforma um Voluntário numa String
     * @return String com toda a informação presente no Voluntário
     */

    public String toString() {
        String s = super.toString();
        StringBuilder sb = new StringBuilder();
        sb.append("Voluntario:");
        sb.append("entregaAtiva=").append(this.entregaAtiva.toString());
        return sb.toString();
    }

    /**
     * Método que determina se um Voluntário e um Objeto são iguais
     * @param o Objeto qualquer
     * @return true caso o model.Voluntario e o Objeto sejam iguais, e vice versa
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Voluntario)) return false;
        if (!super.equals(o)) return false;
        Voluntario v = (Voluntario) o;
        return this.entregaAtiva.equals(v.getEntregaAtiva());
    }


    /**
     * Método que clona um Voluntário, para tal é usado o construtor de cópia
     * @return Objeto model.Voluntario que é uma cópia do Voluntário
     */

    public Voluntario clone() {
        return new Voluntario(this);
    }
}
