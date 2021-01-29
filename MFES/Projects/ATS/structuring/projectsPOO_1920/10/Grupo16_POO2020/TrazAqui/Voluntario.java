package TrazAqui;


import java.io.Serializable;
import java.util.List;

/**
 * Classe que representa um Voluntário TrazAqui!
 */
public class Voluntario extends Entregas implements Serializable {

    /**
     * Construtor vazio de um objeto Voluntario
     */
    public Voluntario(){
        super();
    }

    /**
     * Construtor parametrizado da classe Voluntário
     * @param codUser Código de um Voluntário
     * @param username Nome Completo de um Voluntário
     * @param password Palavra-passe da conta TrazAqui de um Voluntário
     * @param ponto Localização de um Voluntário
     * @param range Raio de ação para entregas, de um Voluntário
     * @param classificacao Classificação de um Voluntário
     * @param numclass Total de Classificações realizadas a um Voluntário
     * @param estado Boolean que demonstra se o Voluntário está apto a realizar uma Entrega
     * @param med Boolean que demonstra se o Voluntário está apto a realizar uma Entrega de medicamentos
     * @param lst Lista que representa o histórico de Encomendas que o Voluntário já realizou
     */
    public Voluntario(String codUser, String username, String password, Ponto ponto, float range, float classificacao, int numclass, boolean estado, boolean med, List<Encomenda> lst){
        super(codUser, username, password, ponto, range, classificacao, numclass, estado,med,lst);
    }

    /**
     * Construtor de cópia da Classe Voluntario
     * @param other Voluntario a copiar
     */
    public Voluntario(Voluntario other){
        super(other);
    }


    /**
     * Método clone
     */
    @Override
    public Voluntario clone() {
        return new Voluntario(this);
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

    @Override
    public void mudaDisponibilidade(boolean state) {

    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("VOLUNTÁRIO\n");
        sb.append(super.toString()).append("\n");
        return sb.toString();
    }

    @Override
    public int compareTo(InterfaceEntregador entregador) {
        return 0;
    }

    public void addEncomenda(Encomenda enc){
        super.addEncomenda(enc);
    }
}
