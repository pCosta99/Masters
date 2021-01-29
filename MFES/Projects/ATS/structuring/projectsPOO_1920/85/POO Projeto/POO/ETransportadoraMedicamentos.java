import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class ETransportadoraMedicamentos extends Empresa_Transportadora implements ETransportadoraMedicamentosI{
    // Variaveis de instancia
    private boolean disponivelParaMedicamentos;

    /**
     * Construtores para objetos da classe ETransportadoraMedicamentos
     */
    public ETransportadoraMedicamentos(){
        super();
        this.disponivelParaMedicamentos = false;
    }

    public ETransportadoraMedicamentos(String codigo, String nome, double gpsX, double gpsY, int nif, double raio, double taxa,
                                       boolean d, double classificacao, double kmsFeitos, int lotacaoEnc, int ocupacaoEnc,
                                       List<Transporte_EncomendaI> es, boolean disponivelMedicamentos){
        super(codigo, nome, gpsX, gpsY, nif, raio, taxa, d, classificacao, kmsFeitos, lotacaoEnc, ocupacaoEnc, es);
        this.disponivelParaMedicamentos = disponivelMedicamentos;
    }

    public ETransportadoraMedicamentos(ETransportadoraMedicamentos vm){
        super(vm);
        this.disponivelParaMedicamentos = vm.isDisponivelParaMedicamentos();
    }

    /**
     * Metodos set, get,
     * equals, clone e toString
     */
    public boolean isDisponivelParaMedicamentos() {
        return this.disponivelParaMedicamentos;
    }

    public void setDisponivelParaMedicamentos(boolean disponivelParaMedicamentos) {
        this.disponivelParaMedicamentos = disponivelParaMedicamentos;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        ETransportadoraMedicamentos et = (ETransportadoraMedicamentos) o;
        return this.disponivelParaMedicamentos == et.isDisponivelParaMedicamentos();
    }

    public ETransportadoraMedicamentos clone(){
        return new ETransportadoraMedicamentos(this);
    }

    public String toString() {
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Empresa transportadora certificada para a entrega de medicamentos\n")
          .append(super.toString())
          .append("Disponivel para medicamentos: ").append(this.disponivelParaMedicamentos).append("\n");
        return  sb.toString();
    }

    /**
     * Metodo que escreve se aceita o transporte de medicamentos
     * Acho que esta mal este metodo
     */
    public void aceitaMedicamentos(boolean state){
        this.clone().setDisponivelParaMedicamentos(state);
        this.clone().setDisponivel(state);
    }

    /**
     * Metodo que diz se aceita o transporte de medicamentos
     * Acho que esta mal este metodo
     */
    public boolean aceitoTransporteMedicamentos(){
        return this.isDisponivelParaMedicamentos();
    }

    /**
     * Metodo que le uma transportadora de medicamentos de uma String
     */
    public void leETM(String cod, String[] p){
        this.setCodigo(cod);
        this.setNome(p[1]);
        this.setGpsX(Double.parseDouble(p[2]));
        this.setGpsY(Double.parseDouble(p[3]));

        this.setNif(Integer.parseInt(p[4]));
        this.setRaio(Double.parseDouble(p[5]));
        this.setTaxa(Double.parseDouble(p[6]));
        this.setDisponivel(Boolean.parseBoolean(p[7]));
        this.setClassificacao(Double.parseDouble(p[8]));
        this.setKmsFeitos(Double.parseDouble(p[9]));
        this.setLotacaoEnc(Integer.parseInt(p[10]));
        this.setOcupacaoEnc(Integer.parseInt(p[11]));

        this.setDisponivelParaMedicamentos(Boolean.parseBoolean(p[12]));

        Transporte_EncomendaI te = new Transporte_Encomenda();
        ArrayList<Transporte_EncomendaI> lte = new ArrayList<>();
        for (int i = 13; i < p.length; i += 4) {
            te.setCodEncomenda(p[i]);
            te.setInicioTransporte(LocalDateTime.parse(p[i+1]));
            te.setFimTransporte(LocalDateTime.parse(p[i+2]));
            te.setCustoTransporte(Double.parseDouble(p[i+3]));

            lte.add(te.clone());
        }
        this.setHe(lte);
        lte.clear();
    }

}
