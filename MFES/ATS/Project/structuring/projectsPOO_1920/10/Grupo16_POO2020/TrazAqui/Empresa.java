package TrazAqui;


import java.io.Serializable;
import java.util.List;
/**
 * Classe que representa uma Empresa TrazAqui!
 */
public class Empresa extends Entregas implements Serializable {
    /**
     * Variável que representa o Preço por Km de uma Empresa
     */
    private float precoKm;
    /**
     *Variável que representa o nif de uma Empresa
     */
    private double nif;

    /**
     * Construtor vazio do objeto Empresa
     */
    public Empresa(){
        super();
        this.precoKm = 0.f;
        nif = 0;
    }
    /**
     * Construtor parametrizado de um objeto Empresa
     * @param codUser Código da empresa
     * @param username Username da empresa
     * @param password Password da empresa
     * @param ponto Localizacão da empresa
     * @param range Range da empresa
     * @param classificacao Classificacão total da empresa
     * @param numclass Número de classificacões da empres
     * @param estado Estado que indica se a empresa esta ou nao apta para realizar uma entrega
     * @param precoKm Quanto custa o servico por km
     * @param med Boolean que indica se a empresa está apta para vender medicamentos
     * @param n NIF da empresa
     * @param hist histórico de entregas da encomenda
     */
    public Empresa(String codUser, String username, String password, Ponto ponto, float range, float classificacao, int numclass, boolean estado, float precoKm, boolean med, List<Encomenda> hist,double n) {
        super(codUser, username, password, ponto, range, classificacao, numclass, estado, med,hist);
        this.precoKm = precoKm;
        this.nif = n;
    }

    /**
     * Construtor copia de um objeto Empresa
     * @param e Empresa a copiar
     */
    public Empresa(Empresa e){
        super(e);
        this.precoKm = e.getPrecoKm();
        this.nif = e.getNif();
    }

    /**
     * Método para obter a variável precoKm
     * @return Preço por Km do serviço
     */
    public float getPrecoKm() {
        return precoKm;
    }

    /**
     * Método para dar set da variável precoKm
     * @param precoKm preço modificado
     */
    public void setPrecoKm(float precoKm) {
        this.precoKm = precoKm;
    }
    /**
     * Método para obter o nif
     * @return double nif
     */
    public double getNif() {
        return nif;
    }

    /**
     * Método para mudar o valor da variável nif.
     * @param nif Nif a registar
     */
    public void setNif(double nif) {
        this.nif = nif;
    }
    /**
     * Método clone
     */
    public Empresa clone(){
        return new Empresa(this);
    }

    /**
     * Método para verificar se um certo objeto é igual a Empresa
     * @param o Objeto a comparar
     * @return true caso sejam iguais e false caso contrário
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Empresa)) return false;
        if (!super.equals(o)) return false;

        Empresa empresa = (Empresa) o;

        if (Float.compare(empresa.getPrecoKm(), getPrecoKm()) != 0) return false;
        return Double.compare(empresa.getNif(), getNif()) == 0;
    }


    /**
     * Método que cria uma String de um objeto em concreto (Neste caso de Empresa)
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("EMPRESA \n");
        sb.append(super.toString());
        sb.append("Preço: ").append(this.precoKm).append("\n");
        sb.append("Nif: ").append(this.nif).append("\n");
        return sb.toString();

    }


    @Override
    public boolean aceitoTransporteMedicamentos() {
        return false;
    }

    @Override
    public void aceitaMedicamentos(boolean state) {

    }

    @Override
    public boolean estaDisponivel() {
        return false;
    }

    /**
     * Método que serve para mudar se uma certa empresa está ou não disponivel para uma entrega
     */
    @Override
    public void mudaDisponibilidade(boolean state) {

    }

    @Override
    public int compareTo(InterfaceEntregador entregador) {
        return 0;
    }



}
