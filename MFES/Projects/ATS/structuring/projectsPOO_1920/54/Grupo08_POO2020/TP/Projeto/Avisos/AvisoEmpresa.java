package Projeto.Avisos;
import Projeto.Interfaces.IAviso;

public class AvisoEmpresa implements IAviso {
    private final boolean aceite;
    private final String idEncomenda;
    private final String idCliente;

    public AvisoEmpresa(boolean aceite, String idEncomenda, String idCliente) {
        this.aceite = aceite;
        this.idCliente = idCliente;
        this.idEncomenda = idEncomenda;
    }

    public AvisoEmpresa(AvisoEmpresa e) {
        this.aceite = e.getAceite();
        this.idEncomenda = e.getIdEncomenda();
        this.idCliente = e.getIdCliente();
    }

    public boolean getAceite() {
        return this.aceite;
    }

    public String getIdEncomenda() {
        return this.idEncomenda;
    }

    public String getIdCliente() {
        return this.idCliente;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("O cliente ").append(this.idCliente).append(" ");
        if(this.aceite) {
            sb.append("aceitou");
        } else {
            sb.append("rejeitou");
        }
        sb.append(" o envio da encomenda ").append(this.idEncomenda).append("!");
        return sb.toString();
    }

    public IAviso clone() {
        return new AvisoEmpresa(this);
    }
}