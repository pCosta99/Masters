import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Empresa_Transportadora extends Transporte implements Empresa_TransportadoraI{
    // Variaveis de instancia
    private int nif;
    private double taxa;
    private double kmsFeitos;
    private int lotacaoEnc;
    private int ocupacaoEnc;
    /**
     * Construtores para objetos da classe Empresa_Transportadora
     */
    public Empresa_Transportadora() {
        super();
        this.nif = 0;
        this.taxa = 0;
        this.kmsFeitos = 0;
        this.lotacaoEnc = 0;
        this.ocupacaoEnc = 0;
    }

    public Empresa_Transportadora(String codigo, String nome, double gpsX, double gpsY, int nif, double raio, double taxa,
                                  boolean d, double classificacao, double kmsFeitos, int ocupacaoEnc, int lotacaoEnc,
                                  List<Transporte_EncomendaI> es) {
        super(codigo, nome, gpsX, gpsY, raio, d, classificacao, es);
        this.nif = nif;
        this.taxa = taxa;
        this.kmsFeitos = kmsFeitos;
        this.lotacaoEnc = lotacaoEnc;
        this.ocupacaoEnc = ocupacaoEnc;
    }

    public Empresa_Transportadora(String cod, String nome, double x, double y) {
        super(cod, nome, x, y);
    }

    public Empresa_Transportadora(Empresa_Transportadora e){
        super(e);
        this.nif = e.getNif();
        this.taxa = e.getTaxa();
        this.kmsFeitos = e.getKmsFeitos();
        this.lotacaoEnc = e.getLotacaoEnc();
        this.ocupacaoEnc = e.getOcupacaoEnc();
    }

    /**
     * Metodos gets e sets,
     * clone, equals e toString
     */
    public int getNif() {
        return this.nif;
    }

    public void setNif(int nif) {
        this.nif = nif;
    }

    public double getTaxa() {
        return this.taxa;
    }

    public void setTaxa(double taxa) {
        this.taxa = taxa;
    }

    public double getKmsFeitos() {
        return kmsFeitos;
    }

    public void setKmsFeitos(double kmsFeitos) {
        this.kmsFeitos = kmsFeitos;
    }

    public int getLotacaoEnc() {
        return this.lotacaoEnc;
    }

    public void setLotacaoEnc(int lotacaoEnc) {
        this.lotacaoEnc = lotacaoEnc;
    }

    public int getOcupacaoEnc() {
        return this.ocupacaoEnc;
    }

    public void setOcupacaoEnc(int ocupacaoEnc) {
        this.ocupacaoEnc = ocupacaoEnc;
    }

    public Empresa_Transportadora clone(){
        return new Empresa_Transportadora(this);
    }

    public String toString(){
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Empresa tranportadora").append("\n")
          .append(super.toString())
          .append("NIF: ").append(this.nif).append("\n")
          .append("Taxa por km: ").append(this.taxa).append("\n")
          .append("Lotacao de encomendas: ").append(this.lotacaoEnc).append("\n")
          .append("Ocupacao das Encomendas: ").append(this.ocupacaoEnc).append("\n");
        return  sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Empresa_Transportadora that = (Empresa_Transportadora) o;
        return nif == that.nif &&
                Double.compare(that.taxa, this.taxa) == 0 &&
                Double.compare(that.kmsFeitos, this.kmsFeitos) == 0 &&
                this.lotacaoEnc == that.lotacaoEnc &&
                this.ocupacaoEnc == that.ocupacaoEnc;
    }

    /**
     * Metodo que determina o preço do transporte de uma encomenda em função da distância e do tempo
     * de espera na loja (estimado ou fornecido pela loja);
     */
    public double precoTransporte(double xL, double yL, double xU, double yU, double peso) {
        double distL, distU, distT, preco;
        distL = Math.sqrt(Math.pow((xL - this.getGpsX()),2) + Math.pow((yL - this.getGpsY()),2));
        distU = Math.sqrt(Math.pow((xU - this.getGpsX()),2) + Math.pow((yU - this.getGpsY()),2));

        if (!this.isDisponivel() && distL > this.getRaio() && distU > this.getRaio())
            return -1;

        distT = distL + Math.sqrt(Math.pow((xU - xL),2) + Math.pow((yU - yL),2));
        if(distL > 100)
            preco = (distT / 2) * this.taxa + peso * 2 * this.taxa;
        else preco = distT * this.taxa + peso * 2 * this.taxa;

        return preco;
    }


    /**
     * Metodo que faz o transporte da encomenda e registar quanto tempo demorou;
     */
    public void entregaEncomendaTr(Transporte_EncomendaI te, double kmsFeitos, char b, int fila, LocalDateTime inicio){
        LocalDateTime aux = somaHoras(inicio, tempoTransporte(fila, kmsFeitos));

        te.setFimTransporte(aux);
        if(b=='S') {
            this.clone().disponivel(true);
        }

        addEncomenda(te);
    }

    /**
     * Metodo que calcula o tempo do transporte
     */
    public LocalTime tempoTransporte(int fila, double kmsFeitos){
        double tempo = (fila + 1) * 5;
        int pMauTempo = prMauTempo();

        if(pMauTempo == 0){
            tempo += (kmsFeitos /45);
        } else tempo += (kmsFeitos / 60);

        int hora = (int)(tempo)/60;
        int minutos = (int) tempo % 60;
        return LocalTime.of(hora,minutos);
    }

    /**
     * Metodo que retorna o total faturado por uma empresa num determinado periodo
     */
    public double totalFaturado(LocalDateTime inicio, LocalDateTime fim){
        double total = 0;
        for(Transporte_EncomendaI te: getHistEntregas())
            if(te.getInicioTransporte().isAfter(inicio) && te.getFimTransporte().isBefore(fim))
                total += te.clone().getCustoTransporte();
        return total;
    }

    /**
     * Metodo que devolve as informacoes de uma Empresa
     */
    public String infTransportadora(){
        StringBuilder sb = new StringBuilder();
        sb
                .append("Codigo - ").append(this.getCodigo()).append("\n")
                .append("Nome - ").append(this.getNome()).append("\n")
                .append("Coordenadas Gps - (").append(this.getGpsX()).append(",").append(this.getGpsY()).append(")\n")
                .append("Raio de ação - ").append(this.getRaio()).append("\n")
                .append("Nif - ").append(this.nif).append("\n")
                .append("Taxa - ").append(this.taxa).append("\n")
                .append("Classificação - ").append(this.getClassificacao()).append("\n")
                .append("Numero de encomendas que pode transportar - ").append(this.lotacaoEnc).append("\n");
        return sb.toString();
    }

    /**
     * Metodo que le uma transportadora de uma String
     */
    public void leTA(String cod, String[] p) {
        this.setCodigo(cod);
        this.setNome(p[1]);
        this.setGpsX(Double.parseDouble(p[2]));
        this.setGpsY(Double.parseDouble(p[3]));
        this.setNif(Integer.parseInt(p[4]));
        this.setRaio(Double.parseDouble(p[5]));
        this.setTaxa(Double.parseDouble(p[6]));

        if(p.length > 7) {
            this.setDisponivel(Boolean.parseBoolean(p[7]));
            this.setClassificacao(Double.parseDouble(p[8]));
            this.setKmsFeitos(Double.parseDouble(p[9]));
            this.setLotacaoEnc(Integer.parseInt(p[10]));
            this.setOcupacaoEnc(Integer.parseInt(p[11]));

            Transporte_EncomendaI te = new Transporte_Encomenda();
            ArrayList<Transporte_EncomendaI> lte = new ArrayList<>();
            for (int i = 12; i < p.length; i += 4) {
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
            this.setKmsFeitos(0);
            this.setLotacaoEnc((int)(Math.random() * 10) % 3 + 1);
            this.setOcupacaoEnc(0);
            this.setHe(new ArrayList<>());
        }

    }

    /**
     * Metodo que atualiza os dados de todas as encomendas da transportadora
     * apos ler os logs todos
     */
    public void atualizaEnc(Map<String, EncomendaI> le){
        EncomendaI e;
        Transporte_EncomendaI aux;
        List<Transporte_EncomendaI> teAux = new ArrayList<>();

        for(Transporte_EncomendaI te: this.getHistEntregas()){
            e = le.get(te.getCodEncomenda()).clone();
            aux = new Transporte_Encomenda(e, te.getInicioTransporte(), te.getFimTransporte(), te.getCustoTransporte());
            teAux.add(aux);
        }
        setHe(teAux);
        teAux.clear();
    }
}
