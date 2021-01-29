package Projeto.Avisos;
import Projeto.Interfaces.IAviso;

public class AvisoEncomendaEntregue implements IAviso {
    private final String idEncomenda;
    private final String idVoluntario;

    public AvisoEncomendaEntregue(String idE, String idVoluntario) {
        this.idEncomenda = idE;
        this.idVoluntario = idVoluntario;
    }

    public AvisoEncomendaEntregue(AvisoEncomendaEntregue a) {
        this.idEncomenda = a.getIdEncomenda();
        this.idVoluntario = a.getIdVoluntario();
    }

    public String getIdEncomenda() {
        return this.idEncomenda;
    }

    public String getIdVoluntario() {
        return this.idVoluntario;
    }

    public String toString() {
        return "A sua encomenda " + this.idEncomenda +" já foi entregue!";
    }

    public IAviso clone() {
        return new AvisoEncomendaEntregue(this);
    }
}