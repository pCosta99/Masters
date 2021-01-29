import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

public class Transporte extends Traz_Aqui_Comum implements TransporteI{
    //Variaveis de instancia
    private double raio;
    private boolean disponivel;
    private double classificacao;
    private List<Transporte_EncomendaI> histEntregas;

    /**
     * Construtores para objetos da classe Voluntario
     */
    public Transporte(){
        this.raio = 0;
        this.disponivel = false;
        this.classificacao = 0;
        this.histEntregas = new ArrayList<>();
    }

    public Transporte(String cod, String nome, double x, double y){
        super(cod, nome, x, y);
    }

    public Transporte(String codigo, String nome, double gpsX, double gpsY, double raio, boolean disponivel,
                      double classificacao, List<Transporte_EncomendaI> he) {
        super(codigo, nome, gpsX, gpsY);
        this.raio = raio;
        this.disponivel = disponivel;
        this.classificacao = classificacao;
        this.histEntregas = new ArrayList<>();
        for (Transporte_EncomendaI te: he)
            this.histEntregas.add(te.clone());
    }

    public Transporte(Transporte t){
        super(t);
        this.raio = t.getRaio();
        this.disponivel = t.isDisponivel();
        this.classificacao = t.getClassificacao();
        this.histEntregas = t.getHistEntregas();
    }

    /**
     * Metodos gets e sets,
     * clone, equlas e toString
     */
    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public boolean isDisponivel() {
        return this.disponivel;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    public double getClassificacao() {
        return classificacao;
    }

    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    public List<Transporte_EncomendaI> getHistEntregas(){
        List<Transporte_EncomendaI> res = new ArrayList<>();
        for(Transporte_EncomendaI te: this.histEntregas)
            res.add(te.clone());
        return res;
    }

    public void setHe(List<Transporte_EncomendaI> tes) {
        this.histEntregas = new ArrayList<>();
        for (Transporte_EncomendaI te : tes)
            this.histEntregas.add(te.clone());
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
                .append("Raio: ").append(this.raio).append("\n")
                .append("Disponivel: ").append(this.disponivel).append("\n")
                .append("Classificacao: ").append(this.classificacao).append("\n");
        if(this.histEntregas.size()>0)
            sb.append("Transporte realizado\n");
        for (Transporte_EncomendaI te: this.histEntregas)
                sb.append(te.toString()).append("\n");

        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Transporte t = (Transporte) o;
        return Double.compare(t.raio, raio) == 0 &&
                disponivel == t.disponivel &&
                Double.compare(t.classificacao, classificacao) == 0 &&
                t.getHistEntregas().equals(this.histEntregas);
    }

    public Transporte clone(){
        return new Transporte(this);
    };

    /**
     * Metodo que sinaliza que está disponivel para recolher encomendas
     */
    public void disponivel(boolean d){
        this.setDisponivel(d);
    }

    /**
     * Metodo que diz se pode ir buscar uma encomenda de um utilizador que é disponibilizada por uma loja;
     */
    public boolean podeIrBuscar(double xL, double yL, double xU, double yU){
        return this.isDisponivel() &&
                (Math.sqrt(Math.pow((xL - this.getGpsX()),2) + Math.pow((yL - this.getGpsY()),2)) <= this.getRaio() &&
                (Math.sqrt(Math.pow((xU - this.getGpsX()),2) + Math.pow((yU - this.getGpsY()),2)) <= this.getRaio()));
    }

    /**
     * Metodo que calcula a distancia a uma loja
     */
    public double distLoja(double xL, double yL){
        return (Math.sqrt(Math.pow((xL - this.getGpsX()),2) + Math.pow((yL - this.getGpsY()),2)));
    }

    /**
     * Atualiza classificacao
     */
    public void atualizaClassificacao(double cl){
        double size = this.histEntregas.size();
        double clTotal = this.classificacao * (size-1);
        this.setClassificacao((cl + clTotal) / (size));
    }

    /**
     * Adiciona uma encomenda ao historico de encomendas
     */
    public void addEncomenda(Transporte_EncomendaI te){
        this.histEntregas.add(te);
    }

    /**
     * Metodo que calcula a distancia percorrida
     */
    public double distTotal(double xL, double yL, double xU, double yU){
        double distLoja = distLoja(xL, yL);
        double distLojaUt = Math.sqrt(Math.pow((xL - xU),2) + Math.pow((yL - yU),2));
        return distLoja + distLojaUt;
    }

    /**
     * Calcula a probabilidade de mau tempo
     */
    protected int prMauTempo(){
        int now = LocalDateTime.now().getDayOfYear();
        boolean inverno = (now < 80 || now > 354);
        boolean primavera = (now >= 80 && now < 172);
        boolean verao = (now >= 172 && now < 266);
        int pMauTempo;

        if(inverno){
            pMauTempo = (int)(Math.random()*100)%2;
        } else if(primavera){
            pMauTempo = (int)(Math.random()*100)%4;
        } else if(verao){
            pMauTempo = (int)(Math.random()*100)%6;
        } else pMauTempo = (int)(Math.random()*100)%3;

        return pMauTempo;
    }

    /**
     * Metodo que soma horas
     */
    public LocalDateTime somaHoras(LocalDateTime inicio, LocalTime time){
        int horaInicio = inicio.getHour();
        int minInicio = inicio.getMinute();
        LocalDate dia = inicio.toLocalDate();
        int diaInicio = inicio.getDayOfYear();
        int ano = inicio.getYear();

        int minFinal = minInicio + time.getMinute();
        int horaFinal = horaInicio + time.getHour();

        if(minFinal > 60){
            minFinal -= 60;
            horaFinal++;
            if(horaFinal > 23){
                diaInicio++;
                horaFinal -= 23;
                dia = LocalDate.ofYearDay(ano, diaInicio);
            }
        }
        time = LocalTime.of(horaFinal, minFinal);

        return LocalDateTime.of(dia, time);
    }


}
