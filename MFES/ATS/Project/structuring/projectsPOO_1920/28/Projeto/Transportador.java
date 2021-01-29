import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Esta classe abstrata guarda a informaçao comum aos Voluntarios e as Transportadoras nao incluida no User
 */
public abstract class Transportador extends User implements Serializable {
    
    private double raio; // raio de resposta
    private Boolean transportemedico; // indica se faz transporte medico
    private double kms; //indica o numero de Kms ja percorridos
    private List<String> paraLevar;   //depois de o utilizador ou o voluntario aceitar levar vai para esta lista
    private List<String> jaFoiBuscar;   //o tranportador ja foi buscar a encomenda á loja agora tem de a levar ao user
    private Boolean disponibilidade; // indica a disponiblilidade de entregar uma encomenda
    private int velocidade; // km/h
    

    public Transportador() {
        super();
        this.raio = 0.0;
        this.transportemedico = false;
        this.kms = 0;
        this.paraLevar = new ArrayList<>();
        this.jaFoiBuscar = new ArrayList<>();
        this.disponibilidade = true;
        this.velocidade = 75;
    }

    public Transportador(String id, String nome ,String password, GPS coordenadas, double raio, Boolean transportemedico) {
        super(id, nome, password,coordenadas);
        this.raio = raio;
        this.transportemedico = transportemedico;
        this.kms = 0;
        this.paraLevar = new ArrayList<>();
        this.jaFoiBuscar = new ArrayList<>();
        this.disponibilidade = true;
        this.velocidade = 75;
    }

    public Transportador(String id, String nome ,String password, GPS coordenadas, double raio, Boolean transportemedico , int velocidade) {
        super(id, nome, password,coordenadas);
        this.raio = raio;
        this.transportemedico = transportemedico;
        this.kms = 0;
        this.paraLevar = new ArrayList<>();
        this.jaFoiBuscar = new ArrayList<>();
        this.disponibilidade = true;
        this.velocidade = velocidade;
    }

    public Transportador (Transportador t) {
        super(t);
        this.raio = t.getRaio();
        this.transportemedico = t.aceitoTransporteMedicamentos();
        this.kms = t.getKms();
        this.paraLevar = new ArrayList<>();
        this.jaFoiBuscar = new ArrayList<>();
        this.disponibilidade = t.getDisponibilidade();
        this.velocidade = t.getVelocidade();
    }


    public void setDisponibilidade (Boolean b){
        this.disponibilidade = b;
    }

    public Boolean getDisponibilidade (){
        return this.disponibilidade;
    }

    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public Boolean aceitoTransporteMedicamentos() {
        return this.transportemedico;
    }

    public void aceitaMedicamentos(Boolean transportemedico) {
        this.transportemedico = transportemedico;
    }

    public double getKms (){
        return this.kms;
    }

    public void setKms(double kms){
        this.kms = kms;
    }

    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Transportador)) {
            return false;
        }
        Transportador transportador = (Transportador) o;
        return super.equals(transportador) &&
                transportador.getRaio() == this.raio && 
                transportador.getVelocidade() == this.velocidade &&
                transportador.aceitoTransporteMedicamentos() == this.transportemedico;
    }


    public List<String> getParaLevar () throws ListaVaziaException {
        if (this.paraLevar.size() == 0){
            throw new ListaVaziaException("Lista Vazia");
        }
        return this.paraLevar;
    }

    
    public List<String> getParaLevarString () {
        return this.paraLevar;
    }

    /**
     * metodo que adiciona uma encomenda a lista de encomendas para levar
     */
    public void addParaLevar (String codEnc){
        this.paraLevar.add(codEnc);
    }

    /**
     * metodo que remove uma encomenda a lista de encomendas para levar
     */
    public void removeParaLevar (String codEnc){
        this.paraLevar.remove(codEnc);
    }


    public int getVelocidade(){
        return this.velocidade;
    }

    public void setVelocidade(int velocidade){
        this.velocidade = velocidade;
    }

    /**
     * metodo que adiciona uma encomenda a lista de encomendas que ja foi buscar a loja
     */
    public void addJaFoiBuscar (String codEnc){
        this.jaFoiBuscar.add(codEnc);
    }

    /**
     * metodo que remove uma encomenda a lista de encomendas que ja foi buscar a loja
     */
    public void removeJaFoiBuscar (String codEnc){
        this.jaFoiBuscar.remove(codEnc);
    }

    public List<String> getJaFoiBuscar (){
        return this.jaFoiBuscar;
    }

    /**
     * metodo que devolve o numero de encomendas que o transportador ja foi buscar a loja
     */
    public int sizeJaFoiBuscar (){
        return this.jaFoiBuscar.size();
    }

    /**
     * metodo que limpa as variaveis paraLevar e jaFoiBuscar e retira da correspondencia a transportadora
     */
    public void limpaListas (Map<String,List<Transportadoras>> correspondencia){
        List<String> ret = this.paraLevar;
        
        this.paraLevar = new ArrayList<>();
        this.jaFoiBuscar = new ArrayList<>();
        
        for (String codEnc : ret){
            List<Transportadoras> t = correspondencia.get(codEnc).stream().filter(x -> !(x.equals(this))).collect(Collectors.toList());
            correspondencia.put(codEnc, t);
        }
    }

    /**
     * metodo que calcula o tempo de chegada a loja
     */
    public double tempoChegadaLoja (Lojas loja , User.Meteorologia tempo ){

        GPS pontoChegada = loja.getCoordenadas();
        double distanciaLoja = super.getCoordenadas().dist(pontoChegada);
        double horasParaChegar = distanciaLoja / this.velocidade ; // vai dar em horas pois velocidade e km/h e dist e em km
        double tempoEmMin = horasParaChegar * 60;

        return tempoEmMin + tempo.getMeteorologia();
    }
    
    /**
     * metodo que calcula o tempo de chegada ao utilizador
     */
    public double tempoChegadaUser (Lojas loja , Utilizador uti , User.Meteorologia tempo){

        GPS pontoChegada = uti.getCoordenadas();
        double distanciaUser = loja.getCoordenadas().dist(pontoChegada);
        double horasParaChegar = distanciaUser / this.velocidade ; // vai dar em horas pois velocidade e km/h e dist e em km
        double tempoEmMin = horasParaChegar * 60;

        return tempoEmMin + tempo.getMeteorologia();
    }

   
}