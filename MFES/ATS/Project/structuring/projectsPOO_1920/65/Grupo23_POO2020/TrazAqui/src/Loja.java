import java.io.Serializable;
import java.util.*;


public class Loja implements Serializable {
    /*Codigo de loja*/
    private String codLoja; //p.e ln

    /*Nome da loja*/
    private String nome; //Loja x

    /*Posicao da loja*/
    private Posicao posicao; //x y

    /*Registo de todas as encomendas*/
    private Map<String, Encomenda> registo;

    /*E um conjunto de encomendas ao entrar na loja*/
    private List<Encomenda> filaDeProcessamento;

    /*E um conjunto de encomendas para sair da loja*/
    private Map<String, Encomenda> filaDeRecolha;

    /**
     * Construtores
     */

    /**
     * Por omissão
     */
    public Loja() {
        this.codLoja = "";
        this.nome = "";
        this.posicao = new Posicao();
        this.registo = new HashMap<>();
        this.filaDeProcessamento = new ArrayList<>();
        this.filaDeRecolha = new TreeMap<>();
    }

    /**
     * Cópia
     */
    public Loja(Loja l) {
        this.codLoja = l.getCodLoja();
        this.nome = l.getNome();
        this.posicao = l.getPosicao();
        this.registo = l.getRegisto();
        this.filaDeProcessamento = l.getFilaDeEspera();
        this.filaDeRecolha = l.getFilaDeRecolha();
    }

    /**
     * Parametrizado
     */
    public Loja(String codLoja, String nome, double x, double y, Map<String, Encomenda> registo, List<Encomenda> filaE, Map<String, Encomenda> filaR) {
        this.codLoja = codLoja;
        this.nome = nome;
        this.posicao = new Posicao(x,y);
        this.registo = registo;
        this.filaDeProcessamento = filaE;
        this.filaDeRecolha = filaR;
    }

    /**
     * Getters
     */
    public Posicao getPosicao() {
        return posicao;
    }

    public String getNome() {
        return nome;
    }

    public String getCodLoja() {
        return codLoja;
    }

    public Map<String, Encomenda> getRegisto() {
        return registo;
    }

    public List<Encomenda> getFilaDeEspera() {
        return filaDeProcessamento;
    }

    public Map<String, Encomenda> getFilaDeRecolha(){return filaDeRecolha;}

    /**
     * Setters
     */
    public void setPosicao(Posicao posicao) {
        this.posicao = posicao;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public void setRegisto(Map<String, Encomenda> registo) {
        this.registo = registo;
    }

    public void setFilaDeEspera(List<Encomenda> filaDeProcessamento) {
        this.filaDeProcessamento = filaDeProcessamento;
    }

    public void setFilaDeRecolha(Map<String, Encomenda> filaDeRecolha){this.filaDeRecolha = filaDeRecolha;}

    /**
     * Adds
     */
    public void addEncsRegistos(Encomenda e){
        registo.put(e.getCodEncomenda(), e);
    }

    public void addEncsQueue(Encomenda e){
        filaDeProcessamento.add(e);
    }

    public void addEncsFilaR(Encomenda e){filaDeRecolha.put(e.getCodEncomenda(), e);}


    /**
     * Separa as encs em encomendas normais
     */
    public List<Encomenda> encsN(){
        List<Encomenda> ret = new ArrayList<>();

        for(Map.Entry<String, Encomenda> m : filaDeRecolha.entrySet()){
            Encomenda e = m.getValue();
            if(!e.getMedica()){
                ret.add(e);
            }
        }

        return ret;
    }

    /**
     * Separa as encs em encomendas Medicas
     */
    public List<Encomenda> encsM(){
        List<Encomenda> ret = new ArrayList<>();

        for(Map.Entry<String, Encomenda> m : filaDeRecolha.entrySet()){
            Encomenda e = m.getValue();
            if(e.getMedica()){
                ret.add(e);
            }
        }

        return ret;
    }


    /**
     * Funçao que sinaliza se existe uma encomenda de um utilizador para ser entregue
     */
    public void sinalizaEntrega(Encomenda e){
        /*Adiciona encomenda a fila de recolha*/
        if(e != null) {
            e.setAceite(true);

            filaDeRecolha.put(e.getCodEncomenda(), e);

            /*Remove encomenda da queue*/
            filaDeProcessamento.remove(e);
        }
    }

    /**
     * Funçao que transforma a map de registo numa lista organizada por data de chegada
     */
    public List<Encomenda> encomendasToList(){
        List<Encomenda> ret = new ArrayList<>();

        for(Encomenda e : registo.values()){
            if(e.getEntregue()) ret.add(e);
        }

        Collections.sort(ret, new ComparatorEncomendaData());

        return ret;
    }


    /**
     * Manipulação
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja loja = (Loja) o;
        return Objects.equals(codLoja, loja.codLoja) &&
                Objects.equals(nome, loja.nome) &&
                Objects.equals(posicao, loja.posicao) &&
                Objects.equals(registo, loja.registo) &&
                Objects.equals(filaDeProcessamento, loja.filaDeProcessamento) &&
                Objects.equals(filaDeRecolha, loja.filaDeRecolha);
    }

    /**
     * Metodo Hashcode
     */
    public int hashCode() {
        return Objects.hash(codLoja, nome, posicao, registo, filaDeProcessamento, filaDeRecolha);
    }

    /**
     * Metodo toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Loja: ").append(codLoja)
                .append("Nome Loja: ").append(nome)
                .append("Posicao: ").append(posicao)
                .append("Registo: ").append(registo)
                .append("Encomendas em espera: ").append(filaDeProcessamento.toString())
                .append("Encomendas para sair: ").append(filaDeRecolha.toString());

        return sb.toString();
    }

    /**
     * Metodo clone
     */
    public Loja clone() {
        return new Loja(this);
    }
}
