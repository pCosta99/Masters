package Projeto.Avisos;
import Projeto.Interfaces.IAviso;

public class AvisoEncomendaAceite implements IAviso {
    private final String idEncomenda;
    private final String idVoluntario;
    private final float tempoPrevisto;

    public AvisoEncomendaAceite(String idC, String idV, float tempoPrevisto) {
        this.idEncomenda = idC;
        this.idVoluntario = idV;
        this.tempoPrevisto = tempoPrevisto;
    }

    public AvisoEncomendaAceite(AvisoEncomendaAceite a) {
        this.idEncomenda = a.getIdEncomenda();
        this.idVoluntario = a.getIdVoluntario();
        this.tempoPrevisto = a.getTempoPrevisto();
    }

    public String getIdEncomenda() {
        return this.idEncomenda;
    }

    public String getIdVoluntario() {
        return this.idVoluntario;
    }

    public float getTempoPrevisto() {
        return this.tempoPrevisto;
    }

    public String toString() {
        return "A sua encomenda " + this.idEncomenda + " será entregue pelo voluntario " + this.idVoluntario
                + "\nEstima-se que o tempo de entrega seja : " + (this.tempoPrevisto/60) + " minutos";
    }

    public IAviso clone() {
        return new AvisoEncomendaAceite(this);
    }
}