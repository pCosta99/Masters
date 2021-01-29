import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

public class Voluntario implements Serializable {
    /*Username do voluntario*/
    private String codVoluntario;

    /*Nome do voluntario*/
    private String nome;

    /*Posicao do voluntario*/
    private Posicao posicao;

    /*Raio de açao do voluntario*/
    private double raio;

    /*Velocidade media do Voluntario*/
    private double vm;

    /*Registo das encomendas*/
    private Map<String, Encomenda> registo;

    /*Voluntario encontra-se disponivel ou nao para fazer entregas*/
    private boolean disponivel;

    /*Voluntario encontra-se disponivel ou nao para fazer entregas medicas*/
    private boolean mdisponivel;

    /*Voluntario e certificado para fazer entregas medicas*/
    private boolean tmedico;

    /*Unica encomenda que vai entregar*/
    private Encomenda encomenda;

    /*Lista de classificaçoes*/
    private List<Float> classificacoes;

    /**
     * Construtores
     */

    /**
     * Por omissão
     */
    public Voluntario() {
        this.posicao = new Posicao();
        this.raio = 0;
        this.vm = 0;
        this.registo = new HashMap<>();
        this.codVoluntario = null;
        this.nome = null;
        this.disponivel = false;
        this.mdisponivel = false;
        this.tmedico = false;
        this.encomenda = new Encomenda();
        this.classificacoes = null;
    }


    /**
     * Cópia
     */
    public Voluntario(Voluntario v) {
        this.posicao = v.getPosicao();
        this.raio = v.getRaio();
        this.vm = v.getVM();
        this.registo = v.getRegisto();
        this.codVoluntario = v.getCodVoluntario();
        this.nome = v.getNome();
        this.disponivel = v.getDisponivel();
        this.mdisponivel = v.getMDisponivel();
        this.tmedico = v.getTMedico();
        this.encomenda = v.getEncomenda();
        this.classificacoes = v.getClassificacoes();
    }

    /**
     * Parametrizado
     */
    public Voluntario(double x, double y, double raio, double vm, Map<String, Encomenda> reg, String user, String name, boolean disponivel, boolean mdisponivel, boolean tmedico, Encomenda encomenda, List<Float> c) {
        this.posicao = new Posicao(x,y);
        this.raio = raio;
        this.vm = vm;

        this.registo = new HashMap<>();
        for (Map.Entry<String, Encomenda> e : reg.entrySet()) {
            this.registo.put(e.getKey(), e.getValue());
        }

        this.codVoluntario = user;
        this.nome = name;
        this.disponivel = disponivel;
        this.mdisponivel = mdisponivel;
        this.tmedico = tmedico;
        this.encomenda = encomenda;
        this.classificacoes = c;
    }

    /**
     * Getters
     */
    public String getCodVoluntario() {
        return codVoluntario;
    }

    public String getNome() {
        return nome;
    }

    public Posicao getPosicao() {
        return posicao;
    }

    public double getRaio() {
        return raio;
    }

    public double getVM(){
        return vm;
    }

    public Map<String, Encomenda> getRegisto() {
        return registo;
    }

    public Encomenda getEncomenda() {
        return encomenda;
    }

    public boolean getDisponivel() {
        return disponivel;
    }

    public boolean getMDisponivel(){
        return mdisponivel;
    }

    public boolean getTMedico(){
        return tmedico;
    }

    public List<Float> getClassificacoes() {
        return classificacoes;
    }

    /**
     * Setters
     */
    public void setCodVoluntario(String codVoluntario) {
        this.codVoluntario = codVoluntario;
    }

    public void setPosicao(Posicao posicao) {
        this.posicao = posicao;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public void setVM(double vm) {
        this.vm = vm;
    }

    public void setRegisto(Map<String, Encomenda> registos) {
        this.registo = registos;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    public void setMdisponivel(boolean mdisponivel) {
        this.mdisponivel = mdisponivel;
    }

    public void setTmedico(boolean tmedico) {
        this.tmedico = tmedico;
    }

    public void setEncomenda(Encomenda encomenda) {
        this.encomenda = encomenda;
    }

    public void setClassificacoes(List<Float> classificacoes) {
        this.classificacoes = classificacoes;
    }

    /**
     * Adds
     */
    public void addRegisto(Encomenda e){
        registo.put(e.getCodEncomenda(), e);
    }

    /**
     * Calcula media de classificaçoes
     */
    public float calculaCF(){
        float total = 0;

        for(int i = 0; i < classificacoes.size(); i++){
            total += classificacoes.get(i);
        }

        return total/classificacoes.size();
    }

    /**
     * Funçao que calcula a media do tempo que demorou na deslocaçao
     */
    public double calculaTDesloc(double dist){
        return dist / vm;
    }

    /**
     * Funçao que faz transporte
     */
    public double fazTrans(Loja l, Utilizador u){

        /*Distancia loja utilizador*/
        double dist = l.getPosicao().distancia(u.getPosicao());

        double temp = calculaTDesloc(dist);

        /*Tempo da loja ate ao utilizador*/
        encomenda.setIntervalo(temp);

        /*Mudança da flag na encomenda para entregue*/
        encomenda.setEntregue(true);

        /*Dar set a hora de entrega*/
        encomenda.setDataDeChegada(LocalDateTime.now());

        /*Encomenda removida do voluntario*/
        encomenda = null;

        return temp;
    }


    public void aceitaMedicamentos(boolean state){
        mdisponivel = state;
    }

    public boolean aceitoTransporteMedicamentos(){
        return mdisponivel;
    }



    /**
     * Funçao que passa o booleano de disponibilidade para string
     */
    public String btoString(boolean b){
        if(!b){
            return "Nao disponivel.";
        }
        else return "Disponivel.";
    }

    public void mudaAvailability(){
        if(!disponivel) disponivel = true;
        else disponivel = false;
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


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Voluntario that = (Voluntario) o;
        return Double.compare(raio, that.raio) == 0 &&
                disponivel == that.disponivel &&
                mdisponivel == that.mdisponivel &&
                tmedico == that.tmedico &&
                Objects.equals(codVoluntario, that.codVoluntario) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(posicao, that.posicao) &&
                Objects.equals(registo, that.registo) &&
                Objects.equals(encomenda, that.encomenda) &&
                Objects.equals(classificacoes, that.classificacoes);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codVoluntario, nome, posicao, raio, registo, disponivel, mdisponivel, tmedico, encomenda, classificacoes);
    }

    /**
     * Manipulação
     */


    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Voluntario: ").append(codVoluntario)
                .append("Nome: ").append(nome)
                .append("Posicao: ").append(posicao)
                .append("Raio: ").append(raio)
                .append("Encomendas Registadas: ").append(registo.toString())
                .append("Encomenda Atual: ").append(encomenda)
                .append("Disponivel: ").append(disponivel)
                .append("Medicamento Disponivel: ").append(mdisponivel)
                .append("Transport Medico: ").append(tmedico)
                .append("Classificaçoes Registadas: ").append(classificacoes.toString());

        return sb.toString();
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }
}
