import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class EmpresaTransportes extends UtilizadorSistema implements Serializable {
    private final int nif;
    private final double custo_km;
    private final int velocidade;
    private int minutosDeEspera;
    private final String local;
    private final double raioDeAcao;
    private double classificao;
    private int avaliacoes;
    private ArrayList<Encomenda> registos;
    private boolean transporteMedico;
    private boolean disponivel;


    public EmpresaTransportes(String email,String password,String codigo, String nome, int nif, double custo_km, String local,double latitude, double longitude, double raioDeAcao, ArrayList<Encomenda> registos,
                              boolean transporteMedico, double classificao, int avaliacoes, boolean disponivel, int minutosDeEspera, int velocidade){
        super(email, password, "Transportadora", codigo, nome, latitude, longitude);
        this.nif = nif;
        this.custo_km = custo_km;
        this.local = local;
        this.raioDeAcao = raioDeAcao;
        this.setRegistos(registos);
        this.transporteMedico = transporteMedico;
        this.classificao = classificao;
        this.avaliacoes = avaliacoes;
        this.disponivel = disponivel;
        this.minutosDeEspera = minutosDeEspera;
        this.velocidade = velocidade;
    }

    public EmpresaTransportes(EmpresaTransportes a){
        super(a);
        this.nif = a.getNif();
        this.custo_km = a.getCusto_km();
        this.local = a.getLocal();
        this.raioDeAcao = a.getRaioDeAcao();
        this.setRegistos(a.getRegistos());
        this.transporteMedico = a.aceitoTransporteMedicamentos();
        this.avaliacoes = a.getAvaliacoes();
        this.classificao = a.getClassificao();
        this.disponivel = a.isDisponivel();
        this.minutosDeEspera = a.getMinutosDeEspera();
        this.velocidade = a.getVelocidade();
    }

    public int getMinutosDeEspera() {
        return this.minutosDeEspera;
    }

    public int getVelocidade() {
        return this.velocidade;
    }

    public boolean aceitoTransporteMedicamentos(){
        return this.transporteMedico;
    }

    public void aceitaMedicamentos(boolean state){
        this.transporteMedico = state;
    }

    public String getCodigo(){
      return super.getCodigo();
    }

    public int getAvaliacoes() {
        return avaliacoes;
    }

    public double getClassificao() {
        return classificao;
    }

    public String getNome(){
      return super.getNome();
    }

    public int getNif(){
      return this.nif;
    }

    public double getCusto_km(){
      return this.custo_km;
    }

    public String getLocal() {
        return local;
    }

    public double getLatitude() {
        return super.getLatitude();
    }

    public double getLongitude() {
        return super.getLongitude();
    }

    public double getRaioDeAcao() {
        return raioDeAcao;
    }

    public boolean isDisponivel() {
        return this.disponivel;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    public ArrayList<Encomenda> getRegistos() {
        return this.registos.stream().map(Encomenda::clone).collect(Collectors.toCollection(ArrayList::new));
    }

    public void setMinutosDeEspera(int minutosDeEspera) {
        this.minutosDeEspera = minutosDeEspera;
    }

    public void setCodigo(String codigo) {
        super.setCodigo(codigo);
    }

    public void setNome(String nome) {
        super.setNome(nome);
    }

    public void setRegistos(ArrayList<Encomenda> registos) {
        this.registos = new ArrayList<>();
        for(Encomenda e: registos){
            this.registos.add(e.clone());
        }
    }

    public EmpresaTransportes clone(){
        return  new EmpresaTransportes(this);
    }

    public boolean equals(Object obj){
        if (obj == this )return true;
        if(obj == null || this.getClass() != obj.getClass()) return false;
        EmpresaTransportes e = (EmpresaTransportes) obj;
        return  super.equals(obj) &&
                this.nif == e.getNif() &&
                this.custo_km == e.getCusto_km() &&
                this.local.equals(e.getLocal()) &&
                this.raioDeAcao == e.getRaioDeAcao() &&
                this.registos.equals(e.getRegistos()) &&
                this.transporteMedico == e.aceitoTransporteMedicamentos();
    }

    public String toString(){
        return super.toString() +
                "Código de empresa: " +
                getCodigo() + "\n" +
                "Nome: " +
                getNome() + "\n" +
                "Nif: " +
                this.nif + "\n" +
                "Custo por km: " +
                this.custo_km + "\n" +
                "Local: " +
                this.local + "\n" +
                "Latitude: " +
                getLatitude() + "\n" +
                "Longitude: " +
                getLongitude() + "\n" +
                "Raio de ação: " +
                this.raioDeAcao + "\n" +
                "Registos de encomendas: " +
                this.registos.toString() +
                "Número mínimo de encomendas: " +
                "Apta para transportes médicos: " +
                this.transporteMedico + "\n";
    }

    /**
     * Método que atualiza a classificação da empressa
     */
    public void updateRate(Double classificacao){
        double total = this.classificao* this.avaliacoes + classificacao;
        this.avaliacoes++;
        this.classificao = total / this.avaliacoes;
    }

    /**
     * Método que adiciona uma encomenda aos registos de encomendas
     */
    public void addEncomenda (Encomenda e){
        this.registos.add(e.clone());
    }

    /**
     * Método que devolve o número de encomendas efetuadas pelo voluntário entre 2 datas
     */
    public String getInfoEncomendas(LocalDateTime d1, LocalDateTime d2){
        StringBuilder sb = new StringBuilder();
        int count = 0;
        Set<String> lojas = new TreeSet<>();
        for(Encomenda e: this.registos){
            LocalDateTime date = e.getData();
            if(date.compareTo(d1) >= 0 && date.compareTo(d2) <= 0){
                lojas.add(e.getCodigo_loja());
                count++;
            }
        }
        String s1 = d1.getDayOfMonth() + "/" + d1.getMonthValue() + "/" + d1.getYear();
        String s2 = d2.getDayOfMonth() + "/" + d2.getMonthValue() + "/" + d2.getYear();

        sb.append("Entre as datas ").append(s1).append(" e ").append(s2).append(" foram realizadas ").append(count).append(" encomendas pela empresa ").append(getNome()).append("\n");
        sb.append("Efetuou encomendas em ").append(lojas.size()).append(" lojas");
        return sb.toString();
    }


    /**
     * Método que devolve o total faturado por uma empresa entre duas datas
     */
    public String getFaturacao(LocalDateTime d1, LocalDateTime d2, BDGeral bd){
        StringBuilder sb = new StringBuilder();
        double total = 0;
        for(Encomenda e: this.registos){
            double unit = 0;
            double kms = 0;
            LocalDateTime date = e.getData();
            if(date.compareTo(d1) >= 0 && date.compareTo(d2) <= 0){
                unit += e.getPeso() * 0.2;
                try {
                    Utilizador userEnc = bd.getUtilizadores().getUsers().get(bd.getUtilizadores().getEmail(e.getCodigo_user())).clone();
                    Loja lojaEnc = bd.getLojas().getLojas().get(bd.getLojas().getEmail(e.getCodigo_loja())).clone();
                    kms += DistanceCalculator.distance(this.getLatitude(), lojaEnc.getLatitude(), this.getLongitude(), lojaEnc.getLongitude());
                    kms += DistanceCalculator.distance(lojaEnc.getLatitude(), userEnc.getLatitude(), lojaEnc.getLongitude(), userEnc.getLongitude());

                    unit += kms * this.getCusto_km();

                } catch (UserNotFoundException enc){
                    System.out.println("User not found");
                } catch (LojaNotFoundException enc){
                    System.out.println("Loja not found");
                }
            }
            total += unit;
        }

        String s1 = d1.getDayOfMonth() + "/" + d1.getMonthValue() + "/" + d1.getYear();
        String s2 = d2.getDayOfMonth() + "/" + d2.getMonthValue() + "/" + d2.getYear();

        sb.append("A sua empresa faturou ").append(total).append(" entre as datas ").append(s1).append(" e ").append(s2);
        return sb.toString();
    }

    /**
     * Método que devolve todos os kms percorridos por uma empresa com todas as suas encomendas
     */
    public int getKms(BDGeral bd){
        int total = 0;
        for(Encomenda e: this.registos){
            try {
                Utilizador u = bd.getUtilizadores().getUsers().get(bd.getUtilizadores().getEmail(e.getCodigo_user())).clone();
                Loja j = bd.getLojas().getLojas().get(bd.getLojas().getEmail(e.getCodigo_loja())).clone();

                double dist1 = DistanceCalculator.distance(this.getLatitude(), j.getLatitude(), this.getLongitude(), j.getLongitude());
                double dist2 = DistanceCalculator.distance(j.getLatitude(), u.getLatitude(), j.getLongitude(), u.getLongitude());
                int kms = (int) (dist1 + dist2);
                total += kms;

            } catch (UserNotFoundException | LojaNotFoundException ignored){
            }
        }

        return total;
    }

    /**
     * Método que devolve as encomendas de uma empresa que já se encontram preparadas para serem entregues
     */
    public String getPreparadas(){
        StringBuilder sb = new StringBuilder();
        int count = 0;
        for(Encomenda s: this.registos){
            if(!s.isEntregue() && !s.isLevantada() && s.isPreparada()){
                sb.append(s);
                count++;
            }
        }
        if(count == 0) sb.append("0");
        return sb.toString();
    }

    /**
     * Método que devolve uma encomenda com código cod
     */
    public Encomenda getEncomenda(String cod) throws EncomendaNotFoundException{
        for(Encomenda s: this.registos){
            if(cod.equals(s.getCodigo())) return s;
        }
        throw new EncomendaNotFoundException();
    }

    /**
     * Método que atualiza uma encomenda como estando já levantada
     */
    public void updateEncomendaLoja(Encomenda enc){
        ArrayList<Encomenda> aux = new ArrayList<>();
        enc.setLevantada(true);
        aux.add(enc);
        for(Encomenda e: this.registos){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setRegistos(aux);
    }

    /**
     * Método que devolve as encomendas não entregues
     */
    public String getNaoEntregue(){
        StringBuilder sb = new StringBuilder();
        int count = 0;
        for(Encomenda s: this.registos){
            if(!s.isEntregue() && s.isLevantada()){
                sb.append(s);
                count++;
            }
        }
        if(count == 0) sb.append("0");
        if(count > 1){
            return "1";
        }
        return sb.toString();
    }

    /**
     * Método que atualiza uma encomenda como estando já entregue
     */
    public void updateEncomenda(Encomenda enc){
        ArrayList<Encomenda> aux = new ArrayList<>();
        enc.setEntregue(true);
        aux.add(enc);
        for(Encomenda e: this.registos){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setRegistos(aux);
    }

    /**
     * Método que atualiza uma encomenda como estando preparada
     */
    public void updateEncomendaPreparada(Encomenda enc){
        ArrayList<Encomenda> aux = new ArrayList<>();
        enc.setPreparada(true);
        aux.add(enc);
        for(Encomenda e: this.registos){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setRegistos(aux);
    }

    /**
     * Método que verifica se uma dada encomenda existe
     */
    public boolean existe(String enc){
        for(Encomenda e: this.registos){
            if(e.getCodigo().equals(enc)) return true;
        }
        return false;
    }

    /**
     * Método que devolve um array com todas as encomendas levantadas e prontas a serem entregues
     */
    public ArrayList<Encomenda> getRota(){
       ArrayList<Encomenda> ret = new ArrayList<>();
       for(Encomenda e: this.registos){
            if(e.isLevantada()) {
               ret.add(e.clone());
            }
        }
        return ret;
    }

    /**
     * Retorna o número de encomendas por entregar
     */

    public int porEntregar(){
        int i = 0;
        for(Encomenda e: this.registos){
            if(e.isLevantada() && !e.isEntregue()) i++;
        }
        return i;
    }

    /**
     * Retorna o número de encomendas por levantar
     */
    public int porLevantar(){
        int i = 0;
        for(Encomenda e: this.registos){
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

    int distanciaEntreLojas(ArrayList<Encomenda> rota, BDGeral bd){
        int total = 0;
        for(int i = 1; i < rota.size(); i++) {
            Encomenda next = rota.get(i);
            Encomenda ant = rota.get(i - 1);
            try {
                Loja anterior = bd.getLojas().getLojas().get(bd.getLojas().getEmail(ant.getCodigo_loja())).clone();
                Loja seguinte = bd.getLojas().getLojas().get(bd.getLojas().getEmail(next.getCodigo_loja())).clone();
                total += DistanceCalculator.distance(anterior.getLatitude(), seguinte.getLatitude(), anterior.getLongitude(), seguinte.getLongitude());
            } catch (LojaNotFoundException ignored) {

            }
        }
        return total;
    }

}
