import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;


public class Voluntario extends Transporta implements Serializable {
    private double tempoMedioKm;
    private boolean temBicicleta;
    private Encomenda encomenda;

     /**
     * Construtor "random" da classe 'Voluntario'.
     * Apenas lhe disponibilizamos alguns parâmetros como o nome, código, etc.
     */
    public Voluntario(String username, String password, String codigo, 
                      String nome, Coordenadas GPS, double raio) {
                        
        super(username, password, codigo, nome, GPS, raio);
        this.temBicicleta = ThreadLocalRandom.current().nextBoolean();


        Random random = new Random();
                
        if (this.temBicicleta){
            this.tempoMedioKm = 5 + random.nextInt(10);}
        else
            this.tempoMedioKm = 10 + random.nextInt(17);

        this.encomenda=null;
    }


    /**
     * Construtor por cópia.
     */
    public Voluntario(Voluntario v) {
        super(v);
        this.tempoMedioKm = v.getTempoMedioKm();
        this.temBicicleta = v.getBicicleta();
        this.encomenda = v.getEncomenda();

    }

    // Getters e Setters

    public double getTempoMedioKm(){
        return this.tempoMedioKm;
    }

    public Encomenda getEncomenda() {
        if(this.encomenda==null) return null;
       return this.encomenda.clone();
    }

    public boolean getBicicleta() {
       return this.temBicicleta;
   }

    public void setTempoMedioKm(double tempoMedio){
        this.tempoMedioKm = tempoMedio;
    }

    public void setEncomenda(Encomenda e) {
        this.encomenda = e.clone();
    }

    public void setBicicleta(boolean b) {
        this.temBicicleta = b;
    }

    public int getTotalEncomendasRealizadas(){
        return super.getTotalEncomendasRealizadas();
    }

    //////////////////////////////////////////////////////// toString, equals e clone ////////////////////////////////////////////////////////

    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("CodVoluntario: ").append(super.getCodigo()).append("\n");
        sb.append("Nome: ").append(super.getNome()).append("\n");
        sb.append("Longitude: ").append(super.getCoordenadas()).append("\n");
        sb.append("Raio: ").append(super.getRaio()).append("\n");

        return sb.toString();
    }

    public Voluntario clone()
    {
        return new Voluntario(this);
    }

    public boolean equals(Object o)
    {
        if (o==this) return true;
        if (o==null || (o.getClass().equals(this.getClass()) == false)) return false;
        Voluntario v = (Voluntario)o;
        return super.equals(o); //<----nao e preciso ter o resto igual?
    }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
        
    
    /**
     * Calcula tempo aproximado que uma encomenda vai demorar a ser entregue
     * 
     * @param enc(Encomenda) a ser entregue, data do pedido, a loja onde estão os produtos e o utilizador que solicitou a encomenda
     * @return Tempo necessário, em média
     */

    public double tempoParaEntrega(LocalDateTime data, Encomenda enc, Loja l, Utilizador u){
        Coordenadas loja_gps = l.getCoordenadas();
        Coordenadas utilizador_gps = l.getCoordenadas();
        double distancia = loja_gps.distancia(utilizador_gps) + loja_gps.distancia(this.getCoordenadas());

        AtrasaEncomenda atraso = new AtrasaEncomenda(data);
        int minutos_atraso = atraso.tempoDeAtraso();

        int fila_loja;
        try {
            fila_loja = l.tempoNaFila();
        } catch (LojaSemInformacaoFila e) {
            fila_loja = 5;
        }

        return minutos_atraso + fila_loja + (distancia * this.tempoMedioKm);
    } 
    
    /**
     * Método da interface Transporta.
     * O voluntário "pega" numa encomenda.
     * @param e A encomenda que o voluntário vai transportar.
     */
    public void addEncomenda(Encomenda e) {
        this.encomenda = e; // Não se faz clone!
        this.setDisponibilidade(false); // fica indisponível quando já está a transportar uma encomenda
    }
    
    /**
     * Método utilizado quando o voluntário entrega a encomenda.
     */
    public void trataEncomendas() {
        Encomenda e = this.encomenda;
        Loja l = super.getSistema().getLoja(e.getCodLoja());
        Utilizador u = super.getSistema().getUtilizador(e.getCodUtilizador());
        LocalDateTime momentoSaida = LocalDateTime.now();

        double duracao = tempoParaEntrega(momentoSaida, e, l, u);
        LocalDateTime dataChegada = momentoSaida.plusMinutes((int)duracao);
        TempoCustoEncomenda tce = new TempoCustoEncomenda((int)duracao, 0, dataChegada);
        this.atualizaHistorico(e, tce);
        u.atualizaHistorico(new EncomendaEntregue(e, tce));
        this.encomenda = null;
        this.setDisponibilidade(true);
    }

    /**
     * Devolve um Map com as encomendas que foram efetuadas num intervalo(com as encomendas podemos calcular as viagens???? queremos um extrato de viagens)
     *
     * @param inicio e fim do intervalo de tempo
     * @return Map com as encomendas feitas nesse intervalo
     */
    public List<Encomenda> ListaExtratoViagens(LocalDateTime inicio, LocalDateTime fim){
        return super.getEncomendasRealizadas().entrySet().stream().
               filter(a->(a.getValue().isAfter(inicio) && a.getValue().isBefore(fim))).
               sorted((a,b) -> a.getValue().compareTo(b.getValue())).
               map(e -> e.getKey()).
               collect(Collectors.toList());
    }

    
}
