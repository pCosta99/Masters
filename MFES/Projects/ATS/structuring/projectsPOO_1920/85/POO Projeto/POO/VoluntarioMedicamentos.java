import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class VoluntarioMedicamentos extends Voluntario implements VoluntarioMedicamentosI{
    // Variaveis de instancia
    private boolean disponivelParaMedicamentos;

    /**
     * Construtores para objetos da classe VoluntarioMedicamentos
     */
    public VoluntarioMedicamentos(){
        super();
        this.disponivelParaMedicamentos = false;
    }

    public VoluntarioMedicamentos(String codigo, String nome, double gpsX, double gpsY, double raio, boolean disponivel,
                                  double classificacao, List<Transporte_EncomendaI> he, boolean disponivelMedicamentos){
        super(codigo, nome, gpsX, gpsY, raio, disponivel, classificacao, he);
        this.disponivelParaMedicamentos = disponivelMedicamentos;
    }

    public VoluntarioMedicamentos(VoluntarioMedicamentos vm){
        super(vm);
        this.disponivelParaMedicamentos = vm.isDisponivelParaMedicamentos();
    }

    /**
     * Metodo get e set,
     * equals clone e toString
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
        VoluntarioMedicamentos vm = (VoluntarioMedicamentos) o;
        return this.disponivelParaMedicamentos == vm.isDisponivelParaMedicamentos();
    }

    public VoluntarioMedicamentos clone(){
        return new VoluntarioMedicamentos(this);
    }

    public String toString() {
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Voluntario certificado para entrega de medicamentos\n")
          .append(super.toString())
          .append("Disponivel para medicamentos: ").append(this.disponivelParaMedicamentos).append("\n");
        return  sb.toString();
    }

    /**
     * Metodo que diz se aceita ou n encomendas de medicamentos
     */
    public void aceitaMedicamentos(boolean state){
        setDisponivelParaMedicamentos(state);
    }

    /**
     * Metodo que retorna se está disponivel para transportar medicamentos.
     */
    public boolean aceitoTransporteMedicamentos(){
        return isDisponivelParaMedicamentos();
    }

    /**
     * Metodo que le de uma String os dados de um voluntario
     * de medicamentos e atualiza o conteudo do mesmo
     */
    public void leVlM(String cod, String[] p) {
        this.setCodigo(cod);
        this.setNome(p[1]);
        this.setGpsX(Double.parseDouble(p[2]));
        this.setGpsY(Double.parseDouble(p[3]));
        this.setRaio(Double.parseDouble(p[4]));

        this.setDisponivel(Boolean.parseBoolean(p[5]));
        this.setClassificacao(Double.parseDouble(p[6]));
        this.setDisponivelParaMedicamentos(Boolean.parseBoolean(p[7]));

        Transporte_EncomendaI te = new Transporte_Encomenda();
        ArrayList<Transporte_EncomendaI> lte = new ArrayList<>();
        for (int i = 8; i < p.length; i += 4) {
            te.setCodEncomenda(p[i]);
            te.setInicioTransporte(LocalDateTime.parse(p[i + 1]));
            te.setFimTransporte(LocalDateTime.parse(p[i + 2]));
            te.setCustoTransporte(Double.parseDouble(p[i + 3]));

            lte.add(te.clone());
        }
        this.setHe(lte);
        lte.clear();
    }

}
