package src.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Classe abstrata que apenas junta código de model.Transportadora e model.Voluntario
 */

public abstract class Distribuidor extends User implements Comparable<Distribuidor> {

    //Variáveis de Instância

    boolean recolhe;
    List<Entrega> historicoEntregas;
    boolean ocupado;
    Classificacao classificacao;
    double raio;

    /**
     * Construtores da classe model.Distribuidor.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Distribuidor.
     */

    public Distribuidor(){
        super();
        this.ocupado = false;
        this.historicoEntregas = new ArrayList<>();
        this.classificacao = new Classificacao();
        this.recolhe = false;
        this.raio = 0;
    }

    /**
     * Construtor parametrizado de model.Distribuidor.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */

    public Distribuidor(String username, String password, String nome, String email, boolean recolhe, List<Entrega> historicoEntregas, Classificacao classificacao, double raio, Ponto localizacao){
        super(username,password, nome,email,localizacao);
        this.recolhe = recolhe;
        this.setHistoricoEntregas(historicoEntregas);
        this.classificacao = new Classificacao(classificacao);
        this.raio = raio;
        this.ocupado = false;
    }

    /**
     * Construtor de cópia de model.Distribuidor.
     * Aceita como parâmetro outro model.Distribuidor e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Distribuidor(Distribuidor d){
        super(d.getUsername(),d.getPassword(),d.getNome(),d.getEmail(),d.getLocalizacao());
        this.recolhe = d.estaARecolher();
        this.historicoEntregas = d.getHistoricoEntregas();
        this.classificacao = d.getClassificacao();
        this.raio = d.getRaio();
        this.ocupado = false;
    }

    /**
     * Atualiza o histórico de Entregas feitas pelo model.Distribuidor
     * @param historicoEntregas novo histórico de entregas feitas pelo model.Distribuidor
     */

    public void setHistoricoEntregas(List<Entrega> historicoEntregas) {
        ArrayList<Entrega> res = new ArrayList<>();

        for(Entrega e : historicoEntregas){
            res.add(e.clone());
        }
        this.historicoEntregas = res;
    }

    /**
     * Método que atualiza a ocupação de um model.Distribuidor (se está carregando seu máximo de entregas)
     * @param b boolean que indica se o model.Distribuidor está ocupado
     */
    protected void setOcupado(boolean b){
        this.ocupado = b;
    }

    /**
     * Atualiza o raio de entregas do model.Distribuidor
     * @param raio double com o novo raio
     */

    public void setRaio(double raio){
        this.raio = raio;
    }

    /**
     * Atualiza a localizacao do model.Distribuidor
     * @param localizacao Objeto model.Ponto com a informação nova
     */

    /**
     * Atualiza o estado de recolha de encomendas do model.Distribuidor
     * @param recolhe novo estado de recolha de encomendas
     */

    public void setRecolhe(boolean recolhe){
        this.recolhe = recolhe;
    }

    /**
     * Atualiza a classificação de um model.Distribuidor
     * @param c nova classificação do model.Distribuidor
     */

    public void setClassificacao(Classificacao c){
        this.classificacao = c.clone();
    }

    /**
     * Devolve o histórico das entregas realizadas pelo model.Distribuidor
     * @return List<model.Entrega> com todas as entregas realizadas pelo model.Distribuidor
     */

    public List<Entrega> getHistoricoEntregas() {
        ArrayList<Entrega> res = new ArrayList<>();

        for(Entrega e : this.historicoEntregas){
            res.add(e.clone());
        }
        return res;
    }

    /**
     * Devolve a Classificação do model.Distribuidor
     * @return Objeto model.Classificacao do model.Distribuidor
     */

    public Classificacao getClassificacao() {
        return this.classificacao.clone();
    }

    /**
     * Devolve o estado de recolha de encomendas
     * @return true caso o model.Distribuidor esteja a recolher, e vice versa
     */

    public boolean estaARecolher() {
        return this.recolhe;
    }

    /**
     * Devolve o raio de entregas do model.Distribuidor
     * @return double que indica o raio de entregas do model.Distribuidor
     */

    public double getRaio() {
        return this.raio;
    }

    /**
     * Método que verifica se o model.Distribuidor está ocupado (carregando seu máximo de entregas)
     * @return boolean que indica a ocupação do model.Distribuidor
     */
    protected boolean isOcupado(){
        return this.ocupado;
    }

    /**
     * Adiciona uma classicação obtida às classificações do model.Distribuidor
     * @param classificacao classificação a ser adicionada ao distribuidor
     */
    public void addClassificacao(String codEnc, double classificacao){
        this.classificacao.addClassificacao(codEnc,classificacao);
    }

    /**
     * Método para adicionar a model.Entrega ativa de um distribuidor
     * @param e model.Entrega a ser adicionada como ativa
     */
    public abstract void addEntregaAtiva(Entrega e);

    /**
     * Adiciona uma entrega ao registo interno de Entregas realizadas pelo model.Dstribuidor
     * @param e Entrega a ser adicionada no histórico do model.Distribuidor
    */    
    public void addEntregaHistorico(Entrega e){
        this.historicoEntregas.add(e.clone());
    }

   /**
     * Indica se uma encomenda pode ser transportada pelo model.Distribuidor
     * @param e Encomenda a ser verificada se pode ou não ser transportada
     * @return boolean que indica se a encomenda verificada pode ou não ser transportada pelo model.Distribuidor
    */
    public boolean podeTransportar(Encomenda e) {
        return !e.is_medicamento() && this.recolhe;
    }

    /**
     * Método que irá clonar um model.Distribuidor
     * @return Objeto model.Voluntario ou model.Transportadora, consoante a classe do objeto que chama o método
     */

    public abstract Distribuidor clone();

    /**
     * Método que determina se um model.Distribuidor e um outro objeto são iguais
     * @param o Objeto qualquer
     * @return boolean consoante a igualdade dos objetos
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Distribuidor d = (Distribuidor) o;
        return this.recolhe == d.estaARecolher() &&
                this.historicoEntregas.equals(d.getHistoricoEntregas()) &&
                this.classificacao.equals(d.getClassificacao()) &&
                this.raio == d.getRaio() && this.ocupado == d.isOcupado();
    }

    /**
     * Método que transforma um model.Distribuidor numa String
     * @return String com toda a informação presente no objeto que a chama
     */

    public String toString() {
        final StringBuilder sb = new StringBuilder("model.Distribuidor{");
        sb.append(super.toString());
        sb.append("recolhe=").append(this.recolhe);
        sb.append(", historicoEntregas=").append(this.historicoEntregas.toString());
        sb.append(", classificacao=").append(this.classificacao.toString());
        sb.append(", raio=").append(this.raio);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método que compara o this model.Distribuidor com outro model.Distribuidor a partir da ordem lexicográfica de seus usernames
     * @param d2 model.Distribuidor com o qual o this será comparado
     * @return inteiro correspondente à comparação da ordem lexicográfica associada
     */

    public int compareTo(Distribuidor d2){
        return this.getUsername().compareTo(d2.getUsername());
    }


}
