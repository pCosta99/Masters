package Projeto.Avisos;
import Projeto.Interfaces.IAviso;

public class AvisoOrcamentoRecebido implements IAviso {
    private final String idEncomenda;
    private final String idEmpresa;
    private final float preco;
    private final float tempoEsperado;

    public AvisoOrcamentoRecebido(String idEnc, String idEmp, float p, float t) {
        this.idEmpresa = idEmp;
        this.idEncomenda = idEnc;
        this.preco = p;
        this.tempoEsperado = t;
    }

    public AvisoOrcamentoRecebido(AvisoOrcamentoRecebido a) {
        this.idEncomenda = a.getIdEncomenda();
        this.idEmpresa = a.getIdEmpresa();
        this.preco = a.getPreco();
        this.tempoEsperado = a.getTempoEsperado();
    }

    public String getIdEncomenda() {
        return this.idEncomenda;
    }

    public String getIdEmpresa() {
        return this.idEmpresa;
    }

    public float getPreco() {
        return this.preco;
    }

    public float getTempoEsperado() {
        return this.tempoEsperado;
    }

    public String toString() {
        return "A empresa " + this.idEmpresa + " está disposta a transportar a sua encomenda " +
                this.idEncomenda + " por " + this.preco + " euros." +
                "\nEstima-se que o tempo de entrega seja : " + (this.tempoEsperado/60) + " minutos";
    }

    public IAviso clone() {
        return new AvisoOrcamentoRecebido(this);
    }
}
