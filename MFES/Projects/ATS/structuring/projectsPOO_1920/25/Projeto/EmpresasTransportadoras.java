
import java.time.LocalDateTime;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.HashSet;
import java.lang.Math;
public class EmpresasTransportadoras extends Transporte implements Login
{
    /**
     *
     */
    
    // variáveis de instância
    private String nif;
    private double precoKm;
    private int maximoEncomendas;
    private Map<String,Encomenda> filaEspera;
    private String password;
    private Map<Double,Encomenda> lucro;
    private double numKms;

    /**
     * Construtor por omissão da classe EmpresasTransportadoras
     */
    public EmpresasTransportadoras()
    {
        super();
        this.nif = new String();
        this.precoKm = 0;
        this.maximoEncomendas = 1;
        this.filaEspera = new LinkedHashMap<>();
        this.password = new String();
        this.lucro = new HashMap<>();
        this.numKms = 0.0;
    }
    
    /**
     * Construtor parametrizado da classe EmpresasTransportadoras
     */
    public EmpresasTransportadoras(String codigo, String nome, GPS localizacao,  String nif, double raio, double preco, 
                                   boolean apto, int maxEnc, Map<String,Encomenda> filaEspera,
                                   List<Integer> avaliacoes, Set<Encomenda> registos, String pass, Map<Double,Encomenda> lucro,double n)
    {
       super(codigo, nome, raio, localizacao, avaliacoes, registos, apto);
       this.setNif(nif);
       this.setPrecoKm(preco);
       this.setMaximoEncomendas(maxEnc);
       this.setFilaEspera(filaEspera);
       this.setPassword(pass);
       this.setLucro(lucro);
       this.setNumKms(n);
    }
    
    /**
     * Construtor de cópia da classe EmpresasTransportadoras
     */
    public EmpresasTransportadoras(EmpresasTransportadoras e)
    {
       super(e); 
       this.setNif(e.getNif());
       this.setPrecoKm(e.getPreco());
       this.setMaximoEncomendas(e.getMaximoEncomendas());
       this.setFilaEspera(e.getFilaEspera());
       this.setPassword(e.getPassword());
       this.setLucro(e.getLucro());
       this.setNumKms(e.getNumKms());
    }
    
    //getters
    public String getNif(){
        return this.nif;
    }

    public double getPreco(){
        return this.precoKm;
    }
    
    public int getMaximoEncomendas(){
        return this.maximoEncomendas;
    }
    
    public Map<String,Encomenda> getFilaEspera(){
        Map<String,Encomenda> ret = new LinkedHashMap<>();
        for (Map.Entry<String,Encomenda> e : this.filaEspera.entrySet()){
            ret.put(e.getKey(), e.getValue().clone());
        }
        return ret;   
    }
    
    public String getPassword(){
        return this.password;
    }

    public double getNumKms(){
        return this.numKms;
    }

    public Map<Double,Encomenda> getLucro(){
        Map<Double,Encomenda> ret = new HashMap<>();
        for (Map.Entry<Double,Encomenda> e : this.lucro.entrySet()){
            ret.put(e.getKey(), e.getValue().clone());
        }
        return ret;
    }
    
    //setters
    public void setNif(String nif){
        this.nif = nif;
    }

    public void setPrecoKm(double preco){
        this.precoKm = preco;
    }
    
    public void setMaximoEncomendas(int maxEnc){
        this.maximoEncomendas = maxEnc;
    }
    
    public void setNumKms(double n){
        this.numKms = n;
    }

    public void setFilaEspera(Map<String,Encomenda> f){
        this.filaEspera = new LinkedHashMap<>();
        f.entrySet().forEach(e -> this.filaEspera.put(e.getKey(), e.getValue().clone()));
    }
    
    public void setPassword(String pass){
        this.password = pass;
    }
    
    public void setLucro(Map<Double,Encomenda> f){
        this.lucro = new HashMap<>();
        f.entrySet().forEach(e -> this.lucro.put(e.getKey(), e.getValue().clone()));
    }
    
    /**
     * Metodo que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public EmpresasTransportadoras clone() {
        return new EmpresasTransportadoras(this);
    }
    
    /**
     *  Metodo que devolve a representaçao em String da EmpresasTransportadora.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("nif: ").append(this.nif).append("\n");
        sb.append("precoKm: ").append(this.precoKm).append("\n");
        sb.append("Maximo de Encomendas: ").append(this.maximoEncomendas).append("\n");
        sb.append("Fila de Espera: ").append(this.filaEspera).append("\n").append("\n");
        sb.append("Numero de Kms percorridos: ").append(this.numKms).append("\n");
        return sb.toString();
    }
    
    /**
     * Metodo que determina se duas empresas transporadoras sao iguais.
     */
    public boolean equals(Object obj) {
      if(this == obj) return true;
      if(obj == null && this.getClass() != obj.getClass()) return false;
      EmpresasTransportadoras e = (EmpresasTransportadoras) obj;     
      return super.equals(e) && this.nif.equals(e.getNif()) &&
              this.precoKm == e.getPreco() &&
              this.maximoEncomendas == e.getMaximoEncomendas() &&
              this.filaEspera.equals(e.getFilaEspera()) &&
              this.lucro.equals(e.getLucro()) &&
              this.numKms == e.getNumKms() ;
    }
    
    /**
     * Método que determina o numero de kms percorridos por uma transportadora.
     */
    public void atualizaKms(double distancia){
        Double km = this.numKms + distancia;
        this.setNumKms(km); 
    }
    
    /**
     * metodo que sinaliza que uma empresa transportadora esta disposta para recolher encomendas.
     */
    public boolean recolheEncomenda(Encomenda e,Lojas l,Utilizadores u){
        int tamanho = this.filaEspera.size();
        if (tamanho <= this.maximoEncomendas && this.getLocalizacao().distancia(this.getLocalizacao(),l.getLocalizaçao()) <= this.getRaio() && this.getLocalizacao().distancia(this.getLocalizacao(),u.getLocalizaçao()) <= this.getRaio()){
            return true;
        }
        return false;
    }
    
    /**
     * Método que faz o transporte da encomenda. 
     */
    public void fazTransporteEncomenda(Encomenda e){
        Set<Encomenda> novo = new HashSet<Encomenda>();
        Iterator<Encomenda> iter = this.getRegistos().iterator();
        while (iter.hasNext()){
            Encomenda enc = iter.next();
            novo.add(enc.clone());
        }
        novo.add(e.clone());
        this.setRegistos(novo);
    }
    
    /**
     * Método que adiciona uma certa encomenda para ser entregue.
     */
    public void adicionaEntrega(Encomenda e,Lojas l, Utilizadores u){
        this.filaEspera.put(u.getCodigo(),e.clone());
    }
    
    /**
     * Método que calcula o preço de transporte de uma encomenda em função da distância e do tempo de espera na loja.
     */
    public Double precoViagem(Encomenda e,Lojas l,Utilizadores u){
        double distanciaT = this.getLocalizacao().distancia(this.getLocalizacao(),l.getLocalizaçao()) + this.getLocalizacao().distancia(l.getLocalizaçao(),u.getLocalizaçao()); //em km
        double tempo = l.getTempoMedioPessoa(); 
        int tamanho = l.numeroPessoasFila();
        double p = 1+e.getPeso()/300;
        double valor = ((distanciaT/50) * this.precoKm) + (p * ((tempo*tamanho)/(tamanho*10))) + e.precoProdutos();
        return valor;
    }
    
    /**
     * Método que adiciona valor resultante do transporte de uma encomenda.
     */
    public void adicionaLucro(Double precoViagem,Encomenda e){
        this.lucro.put(precoViagem,e.clone());
    }

    /**
     * Método que adiciona um registo (quando uma encomenda é entregue ao destinatario).
     */
    public void addRegisto(Encomenda e){
        Set<Encomenda> novo = new HashSet<Encomenda>();
        novo = this.getRegistos();
        novo.add(e.clone());
        this.setRegistos(novo);
    }
    
    /**
     * Método que usa condições atmosféricas (neste caso das 4 estações do ano) como retorno de um valor que terá um certo peso no calculo do tempo de transporte de uma encomenda.
     */
    public double condicoesAtmosfericas(Encomenda e) {
        int ano = e.getData().getYear();

        LocalDateTime primaveraInício = LocalDateTime.of(ano, 03, 20, 00, 00);
        LocalDateTime primaveraFim = LocalDateTime.of(ano, 6, 20, 23, 59);

        LocalDateTime verãoInício = LocalDateTime.of(ano, 06, 21, 00, 00);
        LocalDateTime verãoFim = LocalDateTime.of(ano, 9, 21, 23, 59);

        LocalDateTime outonoInício = LocalDateTime.of(ano, 9, 22, 00, 00);
        LocalDateTime outonoFim = LocalDateTime.of(ano, 12, 20, 23, 59);
    
        if (e.getData().isAfter(primaveraInício) && e.getData().isBefore(primaveraFim))
            return 0.8;

        if (e.getData().isAfter(verãoInício) && e.getData().isBefore(verãoFim))
            return 0.7;

        if (e.getData().isAfter(outonoInício) && e.getData().isBefore(outonoFim))
            return 0.6;

        return 0.4;
    }
    
    /**
     * Método que calcula tempo de transporte de uma encomenda.
     */
    public double tempoDeEntrega(Encomenda e,Lojas l,Utilizadores u) {
        double distancia = this.getLocalizacao().distancia(this.getLocalizacao(),l.getLocalizaçao()) + this.getLocalizacao().distancia(l.getLocalizaçao(), u.getLocalizaçao()); 
        double velocidadeMedia = 60; 
        double tempoFinal = distancia/(velocidadeMedia*condicoesAtmosfericas(e));
        return Math.round(tempoFinal); 
    }
    
    /**
     * Método que calcula quanto dinheiro uma transportadora ganhou num determinado periodo de tempo.
     */
    public double totalDinheiroTempo(LocalDateTime inicio, LocalDateTime fim) {
        double total = 0;
        for (Map.Entry<Double,Encomenda> entry : this.lucro.entrySet()){
            if(entry.getValue().getData().isAfter(inicio) && entry.getValue().getData().isBefore(fim)){
                total += entry.getKey();
            }
        }
        return total;
    }
    
    /**
     * Método que determina se no momento as empresas aceitam transporte de encomendas de medicamentos.
     */
    public boolean aceitoTransporteMedicamentos(){
        return this.getAptoMed();
    }
    
    /**
     * Método que muda o estado de aceitação desse tipo de encomendas.
     */
    public void aceitaMedicamentos(boolean state){
        this.setAptoMed(state);
    }
    
    /**
     * Método que diz qual o tipo de transporte.
     */
    public String tipoTransporte(){
        return "EmpresasTransportadoras";
    }
    
    /**
     * Método que retorna as encomendas realizadas num determinado periodo de tempo.
     */
    public Set<String> getEncomendasTempo(LocalDateTime inicio, LocalDateTime fim){
        Set<String> set = new HashSet<String>();
        for(Encomenda e: this.getRegistos()){
            if(e.getData().isAfter(inicio) && e.getData().isBefore(fim)){
                set.add(e.getCodEncomenda());
            }
        }
        return set;    
    }

    /**
     * Método que verifica credenciais de um login de uma transportadora.
     */
    public boolean verificaCredenciais(String cod, String pass){
        return (this.getCodigo().equals(cod) && this.password.equals(pass));
    }
    
    /**
     * Método que guarda em ficheiro CSV a informação de uma transportadora.
     */
    public String toStringCSV(){
      StringBuilder sb = new StringBuilder();
      sb.append("Transportadora:");
      sb.append(this.getCodigo()).append(",").append(this.getNome()).append(",").append(this.getLocalizacao());
      sb.append(",").append(this.nif).append(",").append(this.getRaio()).append(",").append(this.precoKm);
      return sb.toString();
    }
}