package Projeto.Avisos;
import Projeto.Interfaces.IAviso;

public class AvisoVoluntario implements IAviso {
    private final String idEncomenda;

    public AvisoVoluntario(String idEncomenda) {
        this.idEncomenda = idEncomenda;
    }

    public AvisoVoluntario(AvisoVoluntario a) {
        this.idEncomenda = a.getIdEncomenda();
    }

    public String getIdEncomenda() {
        return this.idEncomenda;
    }

    public String toString() {
        return "Deseja aceitar a encomenda " + this.idEncomenda + "?";
    }

    public IAviso clone() {
        return new AvisoVoluntario(this);
    }
}