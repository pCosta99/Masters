package TrazAqui;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Classe que representa uma Entrega TrazAqui!(Empresa, Voluntario).
 */
public abstract class Entregas extends Login implements InterfaceEntregador, Serializable {
    /**
     * Float que representa o raio de entrega
     */
    private float range;
    /**
     * Pontuação média
     */
    private float classificacao;
    /**
     * Número de classificações feitas por Utilizadores
     */
    private int numclass;
    /**
     * Disponibilidade
     */
    private boolean estado;
    /**
     * Capacidade para aceitar ou não medicamentos
     */
    private boolean aceitaMedicamento;
    /**
     * Lista de Encomendas entregues
     */
    private List<Encomenda> historico;

    /**
     * Construtor vazio de um objeto Entregas
     */
    public Entregas(){
        super();
        this.range = 0.f;
        this.classificacao = 0.f;
        this.numclass = 0;
        this.estado = false;
        this.aceitaMedicamento = false;
        this.historico = new ArrayList<>();
    }

    /**
     * Construtor parametrizado de um objeto Entregas(sendo que é superclasse de Empresa e Voluntário)
     * @param codUser Codigo do objeto Entregas
     * @param username Username do objeto Entregas
     * @param password Password do objeto Entregas
     * @param ponto Ponto que representa a localização do objeto Entregas
     * @param range Float que mostra o range do objeto Entregas
     * @param classificacao Classificação total do objeto Entregas
     * @param numclass Quantas classificações tem um objeto Entregas
     * @param estado Boolean que indica se o objeto Entregas está apto para realizar uma Entrega
     * @param med Boolean que indica se a empresa está apta para vender medicamentos
     * @param his List
     */
    public Entregas(String codUser, String username, String password, Ponto ponto, float range, float classificacao, int numclass, boolean estado,boolean med,List<Encomenda> his){
        super(codUser,username,password,ponto);
        this.range = range;
        this.classificacao = classificacao;
        this.numclass = numclass;
        this.estado = estado;
        this.aceitaMedicamento = med;
        this.historico = his.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    /**
     * Construtor copia de um objeto Entregas
     * @param e Entregas
     */
    public Entregas(Entregas e){
        super(e);
        this.range = e.getRange();
        this.classificacao = e.getClassificacao();
        this.numclass = e.getNumclass();
        this.estado = e.getEstado();
        this.aceitaMedicamento = e.getAceitaMedicamento();
        this.historico = e.getHistorico();
    }

    //Getters e Setters
    /**
     * Método para obter o Range
     * @return float range
     */
    public float getRange() {
        return range;
    }

    /**
     * Método para alterar ao Range
     * @param range raio de Ação
     */
    public void setRange(float range) {
        this.range = range;
    }

    /**
     * Método para obter a Classificacao
     * @return float classificacao
     */
    public float getClassificacao() {
        return classificacao;
    }
    /**
     * Método para dar set da Classificação
     * @param classificacao Classificação
     * */
    public void setClassificacao(float classificacao) {
        this.classificacao = (this.numclass*this.classificacao + classificacao)/++numclass;
    }
    /**
     * Método para obter o numClass
     * @return int numclass
     */
    public int getNumclass() {
        return numclass;
    }

    /**
     * Método para dar set do numClass
     * @param numclass número de Classificações feitas
     */
    public void setNumclass(int numclass) {
        this.numclass = numclass;
    }
    /**
     * Método para obter o estado
     * @return boolean estado
     */
    public boolean getEstado() {
        return estado;
    }

    /**
     * Método para dar set do estado
     * @param estado Disponibilidade
     */
    public void setEstado(boolean estado) {
        this.estado = estado;
    }
    /**
     * Método para verificar se aceitaMedicamentos ou não
     * @return boolean aceitaMedicamento
     */
    public boolean getAceitaMedicamento() {
        return aceitaMedicamento;
    }

    /**
     * Método para dar set de que certa entrega está apta ou não para aceitar medicamentos
     * @param aceitaMedicamento true ou false caso aceite ou não
     */
    public void setAceitaMedicamento(boolean aceitaMedicamento){
        this.aceitaMedicamento = aceitaMedicamento;
    }

    /**
     * Método para obter o histórico de umas entregas
     * @return Arraylist de Encomendas
     */
    public ArrayList<Encomenda> getHistorico() {
        return this.historico.stream().map(Encomenda::clone).collect(Collectors.toCollection(ArrayList::new));
    }

    /**
     * Método que irá dar set de um outro histórico neste
     * @param histo List de Encomendas
     */
    public void setHistorico(List<Encomenda> histo) {
        this.historico = histo.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    /**
     * Método clone
     */
    public abstract Entregas clone();

    /**
     * Método para verificar se um certo objeto é igual a Entregas.
     * @param o objeto a copiar.
     * @return true caso seja igual ou false caso contrário.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Entregas entregas = (Entregas) o;
        return Float.compare(entregas.range, range) == 0 &&
                Float.compare(entregas.classificacao, classificacao) == 0 &&
                numclass == entregas.numclass &&
                estado == entregas.estado&& entregas.getHistorico().size() == getHistorico().size();
    }
    /**
     * Método que cria uma String de um objeto em concreto (Neste caso de Entregas)
     */
    public String toString() {
        return super.toString() +
                "Alcance: " + this.range + "\n" +
                "Classificação média: " + this.classificacao + "\n" +
                "Disponível: " + this.estado + "\n" +
                "Aceita medicamentos: " + this.aceitaMedicamento + "\n";
    }

    /**
     * Método que adiciona uma encomenda ao ArrayList historico
     * @param enc Encomenda que vai ser adicionada ao historico de um objeto Entregas
     */
    public void addEncomenda(Encomenda enc){
        this.historico.add(enc);
    }

    /**
     * Método que verifca que uma certa encomenda enc ou o cod da mesma está
     * no historico das Entregas
     * @param enc or cod Encomenda ou codigo que irá ser usado como parâmetro para verificar se essa Encomenda está
     *            presente no histórico do objeto Entregas
     * @return boolean
     */
    public boolean verificarSeEntregou(Encomenda enc){
        return this.historico.stream().anyMatch(e -> e.equals(enc));
    }

    /**
     * Método que verifca que uma certa encomenda enc ou o cod da mesma está
     * @param cod código da encomenda
     * @return boolean
     */
    public boolean verificarSeEntregou(String cod){
        return this.historico.stream().anyMatch(e -> e.getCod().equals(cod));
    }

}
