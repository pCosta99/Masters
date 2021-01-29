package src.model;

import src.exceptions.EncomendaInvalidaException;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Classe que cria Lojas
 */

public class Loja extends User {

    //variáveis de classe
    private static String codLojaGlobal = "l0";

    //Variáveis de Instância
    private Map<String,Entrega> encomendasAceites;
    private double tempoAtendimento;

    /**
     * Métodos de Classe
     */

    /**
     * Devolve o username de loja gerado para a próxima loja a ser criada
     * @return String com o novo username de loja
     */

    private static String getCodLojaGlobal(){
        String cod = Loja.codLojaGlobal;
        String inc = cod.substring(1);
        int i = Integer.parseInt(inc);
        i = i+1;
        Loja.setCodLojaGlobal("l"+i);
        return cod;
    }

    /**
     * Atualiza o username de loja gerado para a próxima loja a ser criada
     * @param cod String com o novo username de loja que substituirá o atual
     */
    private static void setCodLojaGlobal(String cod){
        Loja.codLojaGlobal = cod;
    }

    /**
     * Atualiza o username de loja a ser atribuido para a loja seguinte, baseado no ultimo codigo atribuído através de logs;
     * @param cod String com o ultimo username de lojas usado nos logs
     */
    public static void updateCodGlobal(String cod){
        String atual = Loja.getCodLojaGlobal();
        int numberAtual = Integer.parseInt(atual.substring(1));
        int numberCod = Integer.parseInt(cod.substring(1));
        if (numberCod > numberAtual) {
            Loja.setCodLojaGlobal("l"+(numberCod+1));
        }
    }


    /**
     * Construtores da classe model.Loja.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Loja.
     */

    public Loja(){
        super();
        this.encomendasAceites = new HashMap<>();
        this.tempoAtendimento = 0;
    }

    /**
     * Construtor parametrizado de model.Loja.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */

    public Loja( String password, String nome, String email, Map<String,Entrega> encomendasAceites,Ponto p){
        super(Loja.getCodLojaGlobal(),password,nome,email,p);
        this.setEncomendasAceites(encomendasAceites);
        this.tempoAtendimento = 0;
    }

    /**
     * Construtor de cópia de model.Loja.
     * Aceita como parâmetro outra model.Loja e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Loja(Loja l){
        super(l.getUsername(),l.getPassword(),l.getNome(),l.getEmail(),l.getLocalizacao());
        this.encomendasAceites = l.getEncomendasAceites();
        this.tempoAtendimento = l.getTempoAtendimento();
    }


    /**
     * Construtor parametrizado de model.loja para criação de instância a partir de logs.
     * Aceita como parâmetros os valores para cada variável de instância.
     */
    public Loja( String cod, String password, String nome, String email, Map<String,Entrega> encomendasAceites,Ponto p){
        super(cod,password,nome,email,p);
        Loja.updateCodGlobal(cod);
        this.setEncomendasAceites(encomendasAceites);
        this.tempoAtendimento = 0;
    }

    /**
     * Métodos de Instância
     */



    /**
     * Realiza o levantmento de uma encomenda de uma loja, i.e., remove a encomenda das referências internas de encomendas aceites da loja
     *  @param codEnc String com o código da encomenda a ser levantada
     *  @return Instância de entrega levantada da loja 
     */
    public Entrega levantaEncomenda(String codEnc){
        Entrega e = this.encomendasAceites.get(codEnc);
        e.setDataLevantamento(LocalDateTime.now());
        return this.encomendasAceites.remove(codEnc);
    }


    /**
     *  Atualiza o mapa de referências de encomendas aceites na loja
     *  @param pedidos Mapa com a referência de código de encomenda associada a uma entrega
     */
    public void setEncomendasAceites(Map<String,Entrega> pedidos) {
        Map<String,Entrega> res = new HashMap<>();
        for(Map.Entry<String,Entrega> e : pedidos.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }
        this.encomendasAceites = res;
    }

    /**
     *  Devolve o mapa de referências de encomendas aceites na loja
     *  @return Mapa com a referência de código de encomenda associada a uma entrega que estão aceites na instancia de loja
     */
    public Map<String,Entrega> getEncomendasAceites() {
        Map<String,Entrega> res = new HashMap<>();

        for(Map.Entry<String,Entrega> e : this.encomendasAceites.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }

        return res;
    }

    /**
     * Atualiza a variável de instância tempo de atendimento na loja
     * @param tempo novo valor da variável de tempo de atendimento da loja
     */
    public void setTempoAtendimento(double tempo){
        this.tempoAtendimento = tempo;
    }



    /**
     * Devolve o valor da variável de instância tempo de atendimento na loja
     * @return valor da variável de tempo de atendimento da loja
     */
    public double getTempoAtendimento(){
        return this.tempoAtendimento;
    }



    /**
     * Devolve o valor da variável de instância tempo de atendimento na loja
     * @return valor da variável de tempo de atendimento da loja
     */
    public double getTempoEspera(){
        return this.encomendasAceites.size() * this.tempoAtendimento;
    }

    /**
     * Adiciona uma encomenda aceite ao mapa de encomendas aceites da loja
     *  @param e Entrega aceite a ser colocada no mapa
     */
    public void aceitaEncomenda(Entrega e){
        this.encomendasAceites.put(e.getEncomenda().getCodEnc(),e.clone());
    }

    /**
     * Remove uma encomenda aceite do mapa de encomendas aceites da loja
     *  @param codEnc Código de encomenda/entrega a ser removida do mapa de encomendas aceites da loja
     */
    public Entrega removeEncomendaAceite(String codEnc) throws EncomendaInvalidaException {
        Entrega e = this.encomendasAceites.remove(codEnc);
        if(e == null) throw new EncomendaInvalidaException("A Encomenda de código " + codEnc + " não foi encontrada!");
        return e;
    }


    /**
     * Método que transforma uma model.Loja numa String
     * @return String com toda a informação presente na model.Loja
     */

    public String toString() {
        String s = super.toString();
        final StringBuilder sb = new StringBuilder("model.Loja{");
        sb.append("encomendasAceites=").append(this.encomendasAceites.toString());
        sb.append("tempoAtendimento=").append(this.tempoAtendimento);
        sb.append('}');
        return s + sb.toString();
    }

    /**
     * Método que determina se uma model.Loja e um Objeto são iguais
     * @param o Objeto qualquer
     * @return true caso a model.Loja e o Objeto sejam iguais, e vice versa
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Loja loja = (Loja) o;
        return this.encomendasAceites.equals(loja.getEncomendasAceites()) &&
                this.tempoAtendimento == loja.getTempoAtendimento();
    }

    /**
     * Método que clona uma model.Loja, para tal é usado o construtor de cópia
     * @return Objeto model.Loja que é uma cópia da model.Loja
     */

    public Loja clone(){
        return new Loja(this);
    }

}
