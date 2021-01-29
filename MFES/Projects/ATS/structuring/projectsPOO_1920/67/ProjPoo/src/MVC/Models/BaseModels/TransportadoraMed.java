package MVC.Models.BaseModels;

import java.io.Serializable;

/**
 * Class para um User do tipo Transportadora que realiza entrega de Encomendas Médicas
 * e onde as funcionalidades que este poderá usar.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */

public class TransportadoraMed extends Transportadora implements Serializable {
    private boolean transMedicamentos;

    /**
     * Construtor de TransportadoraMed por defeito.
     */
    public TransportadoraMed()  {
        super();
        this.transMedicamentos = false;
    }

    /**
     * Construtor de TransportadoraMed parametrizado.
     * @param c Código da TransportadoraMed.
     * @param n Nome da TransportadoraMed.
     * @param x Coordenada X da Localização da TransportadoraMed.
     * @param y Coordenada Y da Localização da TransportadoraMed.
     * @param nif NIF da TransportadoraMed.
     * @param r Raio de Entrega da TransportadoraMed.
     * @param p Preço Por Km da TransportadoraMed.
     * @param tM Boolean que define se a TransportadoraMed realiza Transporte de Medicamentos.
     */
    public TransportadoraMed(String c, String n, double x, double y, String nif, double r, double p, boolean tM){
        super(c,n,x,y,nif,r,p);
        this.transMedicamentos = tM;
    }

    /**
     * Construtor de TransportadoraMed parametrizado.
     * @param c Código da TransportadoraMed.
     * @param n Nome da TransportadoraMed.
     * @param x Coordenada X da Localização da TransportadoraMed.
     * @param y Coordenada Y da Localização da TransportadoraMed.
     * @param nif NIF da TransportadoraMed.
     * @param r Raio de Entrega da TransportadoraMed.
     * @param p Preço por Km da TransportadoraMed.
     * @param tM Boolean que define se a TransportadoraMed realiza Trasnporte de Medicamentos.
     * @param cap Capacidade da TransportadoraMed.
     */
    public TransportadoraMed(String c, String n, double x, double y, String nif, double r, double p, boolean tM, int cap){
        super(c,n,x,y,nif,r,p,cap);
        this.transMedicamentos = tM;
    }

    /**
     * Construtor de TransportadoraMed por Cópia.
     * @param tMed TransportadoraMed a copiar.
     */
    public TransportadoraMed(TransportadoraMed tMed){
        super(tMed);
        this.transMedicamentos = tMed.aceitaTransporteMedicamentos();
    }

    /**
     * Método que retorna se uma TransportadoraMed realiza Transporte de Medicamentos.
     * @return True caso realize, false caso contrário.
     */
    public boolean aceitaTransporteMedicamentos() {
        return this.transMedicamentos;
    }

    /**
     * Método que define se uma TransportadoraMed realiza Transporte de Medicamentos.
     * @param state Boolean que define se a TransportadoraMed realiza Transporte de Medicamentos.
     */
    public void aceitaMedicamentos(boolean state){
        this.transMedicamentos = state;
    }

    /**
     * Método Clone.
     * @return TransportadoraMed clonada.
     */
    public TransportadoraMed clone(){
        return new TransportadoraMed(this);
    }

    /**
     * Método toString.
     * @return String que contém os dados da TransportadoraMed.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append(super.toString()).append("\nTransporta Medicamentos: ")
        .append(this.transMedicamentos).append("\n");

        return sb.toString();
    }

}
