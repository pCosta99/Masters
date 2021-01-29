import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

public class Voluntario extends UtilizadorSistema implements Serializable {
    private boolean disponivel;
    private boolean transporteMedico;
    private final int velocidade;
    private int minutosDeEspera;
    private final LocalDate horaDeRegisto;
    private final double raio_acao;
    private double classificacao;
    private int avaliacoes;
    private List<Encomenda> historico;

    //Construtor de classe por clone
    public Voluntario(Voluntario a){
        super(a);
        this.disponivel = a.getDisponibilidade();
        this.horaDeRegisto = a.getInicio_transporte();
        this.raio_acao = a.getRaio_acao();
        this.classificacao = a.getClassificacao();
        this.avaliacoes = a.getAvaliacoes();
        this.setHistorico(a.getHistorico());
        this.transporteMedico = a.aceitoTransporteMedicamentos();
        this.minutosDeEspera = a.getMinutosDeEspera();
        this.velocidade = a.getVelocidade();
    }

    //Construtor parametrizado
    public Voluntario(String email, String password, String a, String b, boolean c, double d, double e, LocalDate f, double g, List<Encomenda> h, double classificacao,
                      int avaliacoes, boolean transporteMedico, int velocidade, int minutosDeEspera){
        super(email, password, "Voluntario", b, a, d, e);
        this.disponivel = c;
        this.horaDeRegisto = f;
        this.raio_acao = g;
        this.classificacao = classificacao;
        this.avaliacoes = avaliacoes;
        this.setHistorico(h);
        this.transporteMedico = transporteMedico;
        this.velocidade = velocidade;
        this.minutosDeEspera = minutosDeEspera;
    }

    public int getVelocidade() {
        return velocidade;
    }

    public int getMinutosDeEspera() {
        return minutosDeEspera;
    }

    public boolean aceitoTransporteMedicamentos(){
        return this.transporteMedico;
    }

    public void aceitaMedicamentos(boolean state){
        this.transporteMedico = state;
    }

    //Métodos de obtenção de variáveis
    public String getNome(){
      return super.getNome();
    }

    public double getClassificacao() {
        return classificacao;
    }
    public String getCodigo(){
      return super.getCodigo();
    }

    public boolean getDisponibilidade(){
        return this.disponivel;
    }

    public double getLatitude(){
        return super.getLatitude();
    }

    public double getLongitude(){
        return super.getLongitude();
    }

    public double getRaio_acao(){
        return this.raio_acao;
    }

    public LocalDate getInicio_transporte(){
        return this.horaDeRegisto;
    }

    public int getAvaliacoes() {
        return this.avaliacoes;
    }

    public List<Encomenda> getHistorico(){
        List<Encomenda> res = new ArrayList<>();
        for(Encomenda s: this.historico) res.add(s.clone());
        return res;
    }

    //Método de definição de variáveis
    public void setNome(String a){
      super.setNome(a);
    }

    public void setMinutosDeEspera(int minutosDeEspera) {
        this.minutosDeEspera = minutosDeEspera;
    }

    public void setCodigo(String a){
      super.setCodigo(a);
    }

    public void setDisponibilidade(boolean a){
        this.disponivel = a;
    }

    public void setLatitude(double a){
       super.setLatitude(a);
    }

    public void setLongitude(double a){
        super.setLongitude(a);
    }

    public void setHistorico(List<Encomenda> a){
        this.historico = new ArrayList<>();
        for(Encomenda s: a) this.historico.add(s.clone());
    }

    //Método de clonar um objeto
    public Voluntario clone(){
        return new Voluntario(this);
    }

    //public equals
    public boolean equals(Object o){
        if (o == this) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Voluntario v = (Voluntario) o;
        return super.equals(o)
        && this.disponivel ==  v.getDisponibilidade()
        && this.horaDeRegisto.equals(v.getInicio_transporte())
        && this.raio_acao == v.getRaio_acao()
        && this.historico.equals(v.getHistorico());
    }

    public String toString(){
        return super.toString() +
                "Nome: " +
                getNome() + "\n" +
                "Código de voluntário: " +
                getCodigo() + "\n" +
                "Disponível: " +
                this.disponivel + "\n" +
                "Latitude: " +
                getLatitude() + "\n" +
                "Longitude: " +
                getLongitude() + "\n" +
                "Hora de registo: " +
                this.horaDeRegisto + "\n" +
                "Raio de ação: " +
                this.raio_acao + "\n" +
                "Registos de encomendas: " +
                this.historico.toString();
    }

    /**
     * Método que atualiza a classificação
     */
    public void updateRate(Double classificacao){
        double total = this.classificacao * this.avaliacoes + classificacao;
        this.avaliacoes++;
        this.classificacao = total / this.avaliacoes;
    }

    /**
     * Método que adiciona uma encomenda
     */
    public void addEncomenda (Encomenda e){
        this.historico.add(e.clone());
    }

    /**
     * Método que devolve a encomenda com o código cod
     */
    public Encomenda getEncomenda(String cod) throws EncomendaNotFoundException{
        for(Encomenda s: this.historico){
            if(cod.equals(s.getCodigo())) return s;
        }
        throw  new EncomendaNotFoundException();
    }

    /**
     * Método que define uma encomenda como entregue
     */
    public void updateEncomenda(Encomenda enc){
       List<Encomenda> aux = new ArrayList<>();
       enc.setEntregue(true);
       aux.add(enc);
       for(Encomenda e: this.historico){
           if(!e.getCodigo().equals(enc.getCodigo())){
               aux.add(e);
           }
       }
       setHistorico(aux);
    }

    /**
     * Método que define uma encomenda como levantada de uma loja
     */
    public void updateEncomendaLoja(Encomenda enc){
        List<Encomenda> aux = new ArrayList<>();
        enc.setLevantada(true);
        aux.add(enc);
        for(Encomenda e: this.historico){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setHistorico(aux);
    }

    /**
     * Método que define uma encomenda como estando preparada
     */
    public void updateEncomendaPreparada(Encomenda enc){
        List<Encomenda> aux = new ArrayList<>();
        enc.setPreparada(true);
        aux.add(enc);
        for(Encomenda e: this.historico){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setHistorico(aux);
    }


    /**
     * Método que devolve as encomendas ainda não entregues, mas já levantas da loja
     */
    public String getNaoEntregue(){
        StringBuilder sb = new StringBuilder();
        int count = 0;
        for(Encomenda s: this.historico){
            if(!s.isEntregue() && s.isLevantada()){
                sb.append(s);
                count++;
            }
        }
        if(count == 0) sb.append("0");
        return sb.toString();
    }

    /**
     * Método que devolve as encomendas prepradas e prontas a serem levantas
     */
    public String getPreparadas(){
        StringBuilder sb = new StringBuilder();
        int count = 0;
        for(Encomenda s: this.historico){
            if(!s.isEntregue() && !s.isLevantada() && s.isPreparada()){
                sb.append(s);
                count++;
            }
        }
        if(count == 0) sb.append("0");
        return sb.toString();
    }

    /**
     * Método que verifica se uma encomenda existe num voluntário
     */

    public boolean existe(String enc){
        for(Encomenda e: this.historico){
            if(e.getCodigo().equals(enc)) return true;
        }
        return false;
    }

    /**
     * Método que devolve o número de encomendas efetuadas pelo voluntário entre 2 datas
     */
    public String getInfoEncomendas(LocalDateTime d1, LocalDateTime d2){
        StringBuilder sb = new StringBuilder();
        int count = 0;
        Set<String> lojas = new TreeSet<>();
        for(Encomenda e: this.historico){
            LocalDateTime date = e.getData();
            if(date.compareTo(d1) >= 0 && date.compareTo(d2) <= 0){
                lojas.add(e.getCodigo_loja());
                count++;
            }
        }
        String s1 = d1.getDayOfMonth() + "/" + d1.getMonthValue() + "/" + d1.getYear();
        String s2 = d2.getDayOfMonth() + "/" + d2.getMonthValue() + "/" + d2.getYear();

        sb.append("Entre as datas ").append(s1).append(" e ").append(s2).append(" foram realizadas ").append(count).append(" encomendas pelo voluntário ").append(getNome()).append("\n");
        sb.append("Efetuou encomendas em ").append(lojas.size()).append(" lojas");
        return sb.toString();
    }

    /**
     * Retorna o número de encomendas por entregar
     */

    public int porEntregar(){
        int i = 0;
        for(Encomenda e: this.historico){
            if(e.isLevantada() && !e.isEntregue()) i++;
        }
        return i;
    }

    /**
     * Retorna o número de encomendas por levantar
     */
    public int porLevantar(){
        int i = 0;
        for(Encomenda e: this.historico){
            if(!e.isLevantada() && e.isPreparada()) i++;
        }
        return i;
    }

    /**
     * Método que preve alguns atrasos, por causa das condições atmosféricas, e retorna, em minutos, o tempo perdido;
     */
    int calculaAtrasos(){
        Random random = new Random();
        int clima = random.nextInt(100);
        if(clima <= 75){
            return 0;
        }
        else if(clima <= 94){
            return 30;
        }
        return 60;
    }


}
