/**
 * classe que representa uma notificação
 */
package Model;

import java.io.Serializable;

public class Notificacao implements Serializable {
    private String not;
    private int type; // 1 - Default, 2 - Entrega
    private String estCode;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public Notificacao() {
        this.not = "";
        this.type = 0;
    }

    public Notificacao(String not, int type, String estCode) {
        this.not = not;
        this.type = type;
        this.estCode = estCode;
    }

    public Notificacao(Notificacao aux) {
        this.not = aux.getNot();
        this.type = aux.getType();
        this.estCode = aux.getEstCode();
    }

    //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * devolve notificação
     * @return notificação
     */
    public String getNot() {
        return not;
    }

    /**
     * devolve type
     * @return  type
     */
    public int getType() {
        return type;
    }

    /**
     * altera type
     * @param type type
     */
    public void setType(int type) {
        this.type = type;
    }

    /**
     * devolta estasfetaCode
     * @return estafetaCode
     */
    public String getEstCode() {
        return estCode;
    }
}
