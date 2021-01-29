import java.io.Serializable;
import java.util.List;

public class Transportadoras extends Transportador implements Serializable {

    private String nif;             //nif da transportadora
    private double preco_por_km;     //preco que cobra por km
    private int capacidade;         //capacidade da transportadora
    

    public Transportadoras() {
        super();
        this.nif = "Empty";
        this.preco_por_km = 0.0;
        this.capacidade=1;
       
    }

    public Transportadoras(String codEmpresa, String nomeEmpresa, String password, GPS coordenadas, String nif, double raio, double preco_por_km) {
        super(codEmpresa,nomeEmpresa,password,coordenadas,raio,false);
        this.nif = nif;
        this.preco_por_km = preco_por_km;
        this.capacidade = 1;
        
    }

    public Transportadoras(String codEmpresa, String nomeEmpresa, String password, GPS coordenadas, String nif, double raio, double preco_por_km, int velocidade, int capacidade, boolean transportemedico) {
        super(codEmpresa,nomeEmpresa,password,coordenadas,raio,transportemedico,velocidade);
        this.nif = nif;
        this.preco_por_km = preco_por_km;
        this.capacidade = capacidade;
        
    }

    public Transportadoras (Transportadoras t){
        super(t);
        this.nif = t.getNif();
        this.preco_por_km = t.getPreco_por_km();
        this.capacidade=t.getCapacidade();
        
    }

    public String getNif() {
        return this.nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public double getPreco_por_km() {
        return this.preco_por_km;
    }

    public void setPreco_por_km(double preco_por_km) {
        this.preco_por_km = preco_por_km;
    }


    public int getCapacidade(){
        return this.capacidade;
    }

    public void setCapacidade(int capacidade){
        this.capacidade=capacidade;
    }

    /**
     * comparamos todas as variaveis e o super
     */
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Transportadoras)) {
            return false;
        }
        Transportadoras t = (Transportadoras) o;
        return  super.equals(t) &&
               t.getNif().equals(this.nif) &&
               t.getPreco_por_km() == this.preco_por_km &&
               t.getCapacidade() == this.capacidade;

    }
    

    public String toString()  {
        StringBuilder sb = new StringBuilder();
        
        sb.append("\nCodigo Empresa: ").append(super.getId())
        .append("\nNome Empresa: ").append(super.getNome())
        .append("\nCoordenadas: ").append(super.getCoordenadas().toString())
        .append("\nNif: ").append(this.nif)
        .append("\nRaio: ").append(super.getRaio())
        .append("\nPreco_por_km: ").append(preco_por_km)
        .append("\nCapacidade: ").append(this.capacidade)
        .append("\nVelocidade: ").append(super.getVelocidade())
        .append("\nAceita transporte medico: ").append(super.aceitoTransporteMedicamentos().toString())
        .append("\nPara Levar: ").append(super.getParaLevarString())
        .append("\nJa foi buscar : ").append(super.getJaFoiBuscar())
        .append("\nKms percorridos : ").append(super.getKms())
        .append(super.toString());
        
        return sb.toString();
    }

    public Transportadoras clone () {
        return new Transportadoras(this);
    }

    /**
     * metodo usado para imprimir a transportadora em um ficheiro CSV
     */
    public String paraCSV (){

        StringBuilder sb = new StringBuilder();

        sb.append(super.getId()).append(",");
        sb.append(this.getNome()).append(",");
        sb.append(super.getCoordenadas().getLatitude()).append(",");
        sb.append(super.getCoordenadas().getLongitude()).append(",");
        sb.append(this.nif).append(",");
        sb.append(super.getRaio()).append(",");
        sb.append(this.preco_por_km);


        return sb.toString();
    }

    
    /**
     * custo de entrega, vai ver pela distancia total percorrida * preco por km + metade do peso
     */
    public double custoEntrega (GPS loja , GPS utilizador , double peso){

        double distateloja = super.getCoordenadas().dist(loja);
        double distautilizador = super.getCoordenadas().dist(utilizador);
        double kmTotal = distateloja + distautilizador;

        return (kmTotal * this.preco_por_km) + (peso/2);
    }

    /**
     * calculo do tempo para chegar a uma loja a partir da velocidade e distancia
     * depois adicionamos o fator da meteorologia
     */
    public double tempoChegadaLoja (Lojas loja , User.Meteorologia tempo){

        GPS pontoChegada = loja.getCoordenadas();
        double distanciaLoja = super.getCoordenadas().dist(pontoChegada);
        double horasParaChegar = distanciaLoja / super.getVelocidade();// vai dar em horas pois velocidade e km/h e dist e em km
        double tempoEmMin = horasParaChegar * 60;

        return tempoEmMin + tempo.getMeteorologia();
    }

    /**
     * total faturado por uma transportadora a partir dos registos
     */
    public String totalFaturadoTaxas (){
        
        List<RegistoEncomenda> r = this.verRegistosGeral();
        Double sum = 0.0;
        for (RegistoEncomenda atual : r){
            sum += atual.getTaxaDeEntrega();
        }

        return sum.toString();
    }
}