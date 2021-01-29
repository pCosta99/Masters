package MVC.Models.BaseModels;

import java.io.Serializable;
import java.util.Random;

/**
 * Class para um User do tipo Voluntário e onde as funcionalidades
 * que este poderá usar.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */

public class Voluntario extends User implements Serializable {
    private double raio;
    private Classificacao classificacao;
    private boolean estaLivre;
    private double velocidadeMed;

    /**
     * Construtor de Voluntario por defeito.
     */
    public Voluntario() {
        super();
        this.raio = 0;
        this.classificacao = new Classificacao();
        this.estaLivre = true;
        this.velocidadeMed  = 0;
    }

    /**
     * Construtor de Voluntario Parametrizado.
     * @param c Código do Voluntário.
     * @param n Nome do Voluntário.
     * @param x Coordenada X da Localização do Voluntario.
     * @param y Coordenada Y da Localização do Voluntario.
     * @param r Raio de Entrega do Voluntário.
     */
    public Voluntario(String c, String n, double x, double y , double r) {
        super(c, n, x, y);
        this.raio = r;
        this.classificacao = new Classificacao();
        this.estaLivre = true;
        Random aux = new Random();
        this.velocidadeMed = (aux.nextInt(35)+5) + aux.nextDouble();
    }

    /**
     * Construtor de Voluntario de Cópia.
     * @param vol Voluntario a copiar.
     */
    public Voluntario(Voluntario vol) {
        super(vol);
        this.raio = vol.getRaio();
        this.classificacao = vol.getClassificacao();
        this.estaLivre = vol.getEstaLivre();
        this.velocidadeMed = vol.getVelocidadeMed();
    }

    /**
     * Método que retorna o Raio de Entrega de um Voluntario.
     * @return Raio de Entrega.
     */
    public double getRaio() {
        return this.raio;
    }

    /**
     * Método que retorna a Classificação do Voluntario.
     * @return Cópia da Classificação.
     */
    public Classificacao getClassificacao(){
        return this.classificacao.clone();
    }

    /**
     * Método que retorna o Rating de Entrega do Voluntario.
     * @return Rating de Entrega.
     */
    public double getNota() {
        return this.classificacao.getNota();
    }

    /**
     * Método que retorna se um Voluntario se encontra livre para fazer encomendas.
     * @return True caso se encontre livre, false caso contrário.
     */
    public boolean getEstaLivre() {
        return this.estaLivre;
    }

    /**
     * Método que retorna a Velocidade Média de um Voluntario.
     * @return Velocidade Média.
     */
    public double getVelocidadeMed() {
        return velocidadeMed;
    }

    /**
     * Método que define a Velocidade Média de um Voluntario.
     * @param velocidadeMed Velocidade Média.
     */
    public void setVelocidadeMed(double velocidadeMed) {
        this.velocidadeMed = velocidadeMed;
    }

    /**
     * Método que defina se um Voluntario se encontra livre para realizar Entregas.
     * @param estaLivre True para livre, falso caso contrário
     */
    public void setEstaLivre(boolean estaLivre) {
        this.estaLivre = estaLivre;
    }

    /**
     * Método que classifica o Voluntario.
     * @param nota Classificação.
     */
    public void classificaVoluntario(int nota){
        this.classificacao.addNota(nota);
    }

    /**
     * Método toString.
     * @return String com os dados do Voluntario.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Voluntario{\n").append(super.toString()).append("\nRaio: ")
        .append(this.getRaio()).append("\nClassificacao:").append(String.format("%.2f",this.getNota())).append("\n}");
        return sb.toString();
    }

    /**
     * Método Clone.
     * @return Voluntario Clonado.
     */
    public Voluntario clone(){
        return new Voluntario(this);
    }
}
