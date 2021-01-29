package MVC.Models.BaseModels;

import java.io.Serializable;

/**
 * Class para um User do tipo Voluntário que realiza entregas de encomendas Médicas
 * e onde as funcionalidades que este poderá usar.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */

public class VoluntarioMed extends Voluntario implements Serializable {
    private boolean transMedicamentos;

    /**
     * Construtor de VoluntarioMed Parametrizado.
     * @param c Código do VoluntarioMed.
     * @param n Nome do VoluntarioMed.
     * @param x Coordenada X da Localização do VoluntarioMed.
     * @param y Coordenada Y da Localização do VoluntarioMed.
     * @param r Raio de Entrega do VoluntarioMed.
     * @param tM Boolean que define se o VoluntarioMed realiza Transporte de Medicamentos.
     */
    public VoluntarioMed(String c, String n, double x, double y, double r, boolean tM){
        super(c,n,x,y,r);
        this.transMedicamentos = tM;
    }

    /**
     * Construtor de VoluntarioMed por Cópia.
     * @param volM VoluntarioMed a copiar.
     */
    public VoluntarioMed(VoluntarioMed volM){
        super(volM);
        this.transMedicamentos = volM.aceitoTrasnporteMedicamentos();
    }

    /**
     * Método que retorna se um VoluntarioMed realiza Transporte de Medicamentos.
     * @return True caso realize Transporte de Medicamentos, false caso contrário.
     */
    public boolean aceitoTrasnporteMedicamentos(){
        return this.transMedicamentos;
    }

    /**
     * Método Clone.
     * @return VoluntarioMed clonado.
     */
    public VoluntarioMed clone(){
        return new VoluntarioMed(this);
    }

    /**
     * Método toString.
     * @return String com os dados do VoluntarioMed.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append(super.toString()).append("Transporta Medicamentos: ")
        .append(this.transMedicamentos).append("\n");

        return sb.toString();
    }
}
