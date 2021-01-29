import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Voluntario extends Transporte implements VoluntarioI{

    /**
     * Construtores para objetos da classe Voluntario
     */
    public Voluntario(){
        super();
    }

    public Voluntario(String codigo, String nome, double gpsX, double gpsY, double raio,
                      boolean disponivel, double classificacao, List<Transporte_EncomendaI> he) {
        super(codigo, nome, gpsX, gpsY, raio, disponivel, classificacao, he);
    }

    public Voluntario(String cod, String nome, double x, double y) {
        super(cod, nome, x, y);
    }

    public Voluntario(Voluntario v){
        super(v);
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }

    public String toString(){
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Voluntario\n")
                .append(super.toString());
        return sb.toString();
    }

    public boolean equals(Object obj){
        return super.equals(obj);
    }

    /**
     * Metodo que faz o transporte da encomenda e registar quanto tempo demorou;
     */
    public void entregaEncomendaVl(Transporte_EncomendaI te, double kmsFeitos, char b, int fila, LocalDateTime inicio){
        LocalDateTime aux = somaHoras(inicio, tempoTransporte(fila, kmsFeitos));

        te.setFimTransporte(aux);
        if(b=='S') {
            this.clone().disponivel(true);
        }

        addEncomenda(te);
    }
    /**
     * Metodo que calcula o tempo de viagem
     */
    public LocalTime tempoTransporte(int fila, double kmsFeitos){
        double tempo = (fila + 1) * 5;
        int pMauTempo = prMauTempo();

        if(pMauTempo == 0){
            tempo += (kmsFeitos /5);
        } else tempo += (kmsFeitos / 10);

        int hora = (int)(tempo)/60;
        int minutos = (int) tempo % 60;
        return LocalTime.of(hora,minutos);
    }

    /**
     * Metodo que devolve as informacoes de um voluntario
     */
    public String infVoluntario(){
        StringBuilder sb = new StringBuilder();
        sb
                .append("Codigo - ").append(this.getCodigo()).append("\n")
                .append("Nome - ").append(this.getNome()).append("\n")
                .append("Coordenadas Gps - (").append(this.getGpsX()).append(",").append(this.getGpsY()).append(")\n")
                .append("Raio de ação - ").append(this.getRaio()).append("\n")
                .append("Classificação - ").append(this.getClassificacao()).append("\n");
        return sb.toString();
    }

    /**
     * Metodo que le de uma String os dados de um voluntario
     * e atualiza o conteudo do mesmo
     */
    public void leTA(String cod, String[] p){
        this.setCodigo(cod);
        this.setNome(p[1]);
        this.setGpsX(Double.parseDouble(p[2]));
        this.setGpsY(Double.parseDouble(p[3]));
        this.setRaio(Double.parseDouble(p[4]));

        if(p.length > 5) {
            this.setDisponivel(Boolean.parseBoolean(p[5]));
            this.setClassificacao(Double.parseDouble(p[6]));

            Transporte_EncomendaI te = new Transporte_Encomenda();
            List<Transporte_EncomendaI> lte = new ArrayList<>();
            for (int i = 7; i < p.length; i += 4) {
                te.setCodEncomenda(p[i]);
                te.setInicioTransporte(LocalDateTime.parse(p[i+1]));
                te.setFimTransporte(LocalDateTime.parse(p[i+2]));
                te.setCustoTransporte(Double.parseDouble(p[i+3]));

                lte.add(te.clone());
            }
            this.setHe(lte);
            lte.clear();
        } else {
            this.setDisponivel(true);
            this.setClassificacao(0);
            this.setHe(new ArrayList<>());
        }
    }

    /**
     * Metodo que no final de ler tudo atualiza os dados
     * de cada Transporte de encomenda do historico do utilizador
     */
    public void atualizaEnc(Map<String, EncomendaI> le){
        EncomendaI e;
        Transporte_EncomendaI aux;
        List<Transporte_EncomendaI> teAux = new ArrayList<>();

        for(Transporte_EncomendaI te: this.getHistEntregas()){
            e = le.get(te.getCodEncomenda()).clone();
            aux = new Transporte_Encomenda(e, te.getInicioTransporte(), te.getFimTransporte(), te.getCustoTransporte());
            teAux.add(aux.clone());
        }
        setHe(teAux);
        teAux.clear();
    }
}
