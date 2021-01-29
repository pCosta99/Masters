import java.io.*;
/**
 * Escreva a descrição da classe EmpresaTransportadora aqui.
 * 
 * @author (seu nome)
 * @version (número de versão ou data)
 */

public class EmpresaTransportadora extends Join implements Certificados,Serializable{
    private String nomeEmpresa;
    private boolean estado;
    private double custoTransporte; // taxa fixa
    private double taxa;// custo preco/km
    private int capacidadeEncomenda; //se transporta 1 ou N encomendas
    private double tempoEntrega; //tempo de entrega de uma encomenda
    private double classificacao;
    private double velocidade;
    private double raio;
    private String codEmp; //codigo de Empresa
    private String nif; 
    private boolean certificado;
    private int numViagens;
    private int numKms;
    
    /**
     * construtores
     */
    public EmpresaTransportadora(){
        super();
        this.nomeEmpresa = "";
        this.estado = true;
        this.custoTransporte = 0.0;
        this.taxa = 0.0;
        this.capacidadeEncomenda = 0;
        this.tempoEntrega = 0.0;
        this.classificacao = 0.0;
        this.velocidade = 0.0;
        this.raio = 0.0;
        this.codEmp = "";
        this.nif="";
        this.certificado = false;
        this.numViagens=1;
        this.numKms=0;
    }
    
    public EmpresaTransportadora(String nomeEmpresa,Localizacao localizacao,boolean estado,
                                double custoTransporte,double taxa,int capacidadeEncomenda,Double tempoEntrega,
                                double classificacao,double velocidade,double raio,
                                String codEmp,String nif,boolean certificado,int numViagens,int numKms, String user, String pass){
        super(localizacao, user, pass);
        this.nomeEmpresa = nomeEmpresa;
        this.estado = estado;
        this.custoTransporte = custoTransporte;
        this.taxa = taxa;
        this.capacidadeEncomenda = capacidadeEncomenda;
        this.tempoEntrega = tempoEntrega;
        this.classificacao = classificacao;
        this.velocidade = velocidade;
        this.raio = raio;
        this.codEmp=codEmp;
        this.nif=nif;
        this.certificado = certificado;
        this.numViagens=numViagens;
        this.numKms=numKms;
    }
    
    public EmpresaTransportadora(EmpresaTransportadora empTransp){
        super(empTransp.getLocalizacao(), empTransp.getUser(), empTransp.getPass());
        this.nomeEmpresa = empTransp.getNomeEmpresa();
        this.estado = empTransp.getEstado();
        this.custoTransporte = empTransp.getCustoTransporte();
        this.taxa = empTransp.getTaxa();
        this.capacidadeEncomenda = empTransp.getCapaEncomenda();
        this.tempoEntrega = empTransp.getTempoEntrega();
        this.classificacao = empTransp.getClassificacao();
        this.velocidade = empTransp.getVelocidade();
        this.raio = empTransp.getRaio();
        this.codEmp=empTransp.getCodEmp();
        this.nif=empTransp.getNif();
        this.certificado = empTransp.getCertificado();
        this.numViagens=empTransp.getNumViagens();
        this.numKms=empTransp.getNumKms();
    }
    
    /**
     * Gets
     */
    
    public String getNomeEmpresa(){
        return this.nomeEmpresa;
    }
    
    
    public boolean getEstado(){
        return this.estado;
    }
    
    public double getCustoTransporte(){
        return this.custoTransporte;
    }
    
    public double getTaxa(){
        return this.taxa;
    }
    
    public int getCapaEncomenda(){
        return this.capacidadeEncomenda;
    }
    
    
    public Double getTempoEntrega(){
        return this.tempoEntrega;
    }
    
    public double getClassificacao(){
        return this.classificacao;
    }
    
    public double getVelocidade(){
        return this.velocidade;
    }
    
    public double getRaio(){
        return this.raio;
    }
    
    
    public String getCodEmp(){
        return this.codEmp;
    }
   
    public String getNif(){
        return this.nif;
    }
    
    public boolean getCertificado(){
        return this.certificado;
    }
    
    
    public int getNumViagens(){
        return this.numViagens;
    }
    
    public int getNumKms(){
        return this.numKms;
    }
    
    /**
     * Sets
     */
    
    public void setNomeEmpresa(String nomeEmpresa){
        this.nomeEmpresa = nomeEmpresa;
    }
    
    
    public void setEstado(boolean estado){
        this.estado = estado;
    }
    
    public void setCustoTransporte(double custoTransporte){
        this.custoTransporte = custoTransporte;
    }
    
    public void setTaxa(double taxa){
        this.taxa = taxa;
    }
    
    public void setCapaEntrega(int capacidadeEntrega){
        this.capacidadeEncomenda = capacidadeEntrega;
    }
    
    
    public void setTempoEntrega(Double tempoEntrega){
        this.tempoEntrega = tempoEntrega;
    }
    
    public void setClassificacao(double classificacao){
        this.classificacao = classificacao;
    }
    
    public void setVelocidade(double velocidade){
        this.velocidade = velocidade;
    }
    
    public void setRaio(double raio ){
        this.raio = raio;
    }
    
    public void setCodEmp(String codEmp){
       this.codEmp = codEmp;
    }
   
    public void setNif(String nif){
        this.nif = nif;
    }
    
     public void setCertificado(boolean certificado){
        this.certificado = certificado;
    }
    
    
    public void setNumViagens(int numViagens){
       this.numViagens = numViagens;
    }
    
    public void setNumKms(int numKms){
       this.numKms= numKms;
    }
    
    /**
     * Método clone
     */
    
    public EmpresaTransportadora clone(){
        return new EmpresaTransportadora(this);
    }
    
    /**
     * Método equals
     */
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        EmpresaTransportadora et = (EmpresaTransportadora ) obj;
        return (super.equals(et) &&
                et.getNomeEmpresa().equals(this.nomeEmpresa) &&
                et.getEstado() == (this.estado) &&
                et.getCustoTransporte() == this.custoTransporte &&
                et.getTaxa() == this.taxa &&
                et.getCapaEncomenda() == this.capacidadeEncomenda &&
                et.getTempoEntrega().equals(this.tempoEntrega) &&
                et.getClassificacao() == this.classificacao &&
                et.getVelocidade() == this.velocidade &&
                et.getRaio() == this.raio &&
                et.getCodEmp() == this.codEmp &&
                et.getNif() == this.nif) &&
                et.getNumViagens() == this.numViagens &&
                et.getNumKms() == this.numKms;
    }
    
    
    /**
     * Método toString
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append ("Nome da Empresa: ").append (this.nomeEmpresa);
        sb.append ("\nCódigo da Empresa: ").append (this.codEmp);
        sb.append ("\nLocalização: ").append (this.getLocalizacao());
        sb.append ("\nNif: ").append (this.nif);
        sb.append ("\nRaio: ").append (this.raio);
        sb.append ("\nPreço por Km: ").append (this.taxa);
        sb.append ("\nDisponivel: ").append (this.estado);
        sb.append ("\nCusto do Transporte: ").append (this.custoTransporte);
        sb.append ("\nCapacidade de Encomenda: ").append (this.capacidadeEncomenda);
        sb.append ("\nTempo de Entrega: ").append (this.tempoEntrega);
        sb.append ("\nClassificação: ").append (this.classificacao);
        sb.append ("\nVelocidade: ").append (this.velocidade);
        sb.append ("\nNúmero de Viagens: ").append(this.numViagens);
        sb.append ("\nNúmero de Kms: ").append(this.numKms);
        
       
        
        return sb.toString();
        
        
    }
    
    public boolean aceitoTransporteMedicamentos(){
        return this.certificado;
    }
    
    public void aceitaMedicamentos(boolean state){
        this.certificado = state;
    }

	public void classificar(double cl) {
        this.setClassificacao((numViagens*getClassificacao()+ cl)/(numViagens+1));
        this.setNumViagens(numViagens+1);
    }
    
    public void adicionarKMS(double kms){
        this.numKms += kms;
    }
}

