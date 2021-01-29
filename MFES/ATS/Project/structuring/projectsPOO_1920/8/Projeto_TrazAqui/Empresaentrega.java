import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Empresaentrega extends User implements Serializable {
     
    // variáveis de instância
    private String nif;
    private boolean prontaReceber;
    private double taxa;
    private int capacidade;
    private double velocidade;
    private double classificacao;
    private int nclass;
    private double raio;
    private double distancia;
    private boolean vistoMedico;
    private List<Encomenda> encomendas;

    /**
     * Construtor parametrizado para o ficheiro de logs
     */

    public Empresaentrega(){}

    public Empresaentrega(String username, String nome, Coordenadas pos, String nif,double raio,double precokm){
        super(username,nome,pos);
        this.nif = nif;
        this.raio = raio;
        this.taxa = precokm;
        this.encomendas = new ArrayList<>();
        this.prontaReceber = true;
        this.vistoMedico = false;
        this.nclass = 0;
        this.classificacao = 0;
        this.capacidade = 1;
        this.velocidade = 50;
        this.distancia = 0;
    }

    /**
     * Construtor parametrizado da classe Voluntario.
     * @param nome
       @param user
     * @param password
     * @param posicao
     * @param pR
     * @param taxa
     * @param capacidade
     * @param vel
     * @param classi
     * @param raio
     * @param vM
     * @param enc
     * @return
     */
      public Empresaentrega(String user,String nome,String password,Coordenadas posicao,String nif,boolean pR,double taxa,int capacidade, double vel,double classi,int ncl, double raio,double distancia,boolean vM, List<Encomenda> enc){
        super(user,nome,password,posicao);
        this.nif = nif;
        this.prontaReceber = pR;
        this.taxa = taxa;
        this.capacidade = capacidade;
        this.velocidade = vel;
        this.classificacao = classi;
        this.nclass = ncl;
        this.raio = raio;
        this.distancia = distancia;
        this.vistoMedico = vM;
        this.encomendas = enc.stream().map(Encomenda :: clone).collect(Collectors.toList());
    }
         
     /**
     * Construtor de cópia de Empresaentrega.
     * @param emp
     * @return
     */
     public Empresaentrega(Empresaentrega emp)
        {
        super(emp);
        this.nif = emp.getNif();
        this.prontaReceber = emp.getProntaReceber();
        this.taxa = emp.getTaxa();
        this.capacidade = emp.getCapacidade();
        this.velocidade = emp.getVelocidade();
        this.classificacao = emp.getClassificacao();
        this.nclass = emp.getNclass();
        this.raio = emp.getRaio();
        this.distancia = emp.getRaio();
        this.vistoMedico = emp.getVistoMedico();
        setEncomendas(emp.getEncomendas());
    }
     
     /**
     * Indica se a empresa esta pronta a receber.
     * @param
     * @return prontaReceber
     */
    public boolean getProntaReceber() {
        return prontaReceber;
    }
     
     /**
     * Atualiza se a empresa esta pronta a receber.
     * @param prontaReceber
     * @return
     */
    public void setProntaReceber(boolean prontaReceber) {
        this.prontaReceber = prontaReceber;
    }
     
     /**
     * Indica a taxa da empresa.
     * @param
     * @return taxa
     */
    public double getTaxa() {
        return taxa;
    }
     
     /**
     * Atualiza a taxa da empresa.
     * @param taxa
     * @return
     */
    public void setTaxa(double taxa) {
        this.taxa = taxa;
    }
     
     /**
     * Indica a capacidade da empresa.
     * @param
     * @return capacidade
     */
    public int getCapacidade() {
        return capacidade;
    }
     
     /**
     * Atualiza a capacidade da empresa.
     * @param capacidade
     * @return
     */
    public void setCapacidade(int capacidade) {
        this.capacidade = capacidade;
    }
     
     /**
     * Indica a velocidade da empresa a entregar.
     * @param
     * @return velocidade
     */
    public double getVelocidade() {
        return velocidade;
    }
     
     /**
     * Atualiza a velocidade da empresa a entregar.
     * @param velocidade
     * @return
     */
    public void setVelocidade(double velocidade) {
        this.velocidade = velocidade;
    }
     
     /**
     * Indica a classificacao da empresa de entrega.
     * @param
     * @return classificacao
     */
    public double getClassificacao() {
        return classificacao;
    }
     
     /**
     * Atualiza a classificacao da empresa de entrega.
     * @param classificacao
     * @return
     */
    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    public int getNclass() {
        return nclass;
    }

    public void setNclass(int nclass) {
        this.nclass = nclass;
    }

    /**
     * Indica o raio que a empresa faz a entrega.
     * @param
     * @return raio
     */
    public double getRaio() {
        return raio;
    }
     
     /**
     * Atualiza o raio que a empresa faz a entrega.
     * @param raio
     * @return
     */
    public void setRaio(double raio) {
        this.raio = raio;
    }

    public double getDistancia() {
        return distancia;
    }

    public void setDistancia(double distancia) {
        this.distancia = distancia;
    }

    /**
     * Indica se a empresa entrega material medico(se pode ou nao entregar).
     * @param
     * @return vistomedico
     */
    public boolean getVistoMedico() {
        return vistoMedico;
    }
     
     /**
     * Atualiza se a empresa entrega material medico(se pode ou nao entregar).
     * @param vistoMedico
     * @return
     */
    public void setVistoMedico(boolean vistoMedico) {
        this.vistoMedico = vistoMedico;
    }

    public String getNif() {
        return nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    /**
     * Indica a lista de encomendas que a empresa tem
     * @param
     * @return  List<Encomenda> Encomendas
     */
    public List<Encomenda> getEncomendas(){
        List<Encomenda> aux = new ArrayList<Encomenda>();
        for(Encomenda e : this.encomendas){
            aux.add(e.clone());
        }
        return aux;
    }
     
     /**
     * Atualiza a lista de encomendas
     * @param lista
     * @return
     */
    public void setEncomendas(List<Encomenda> lista){
        this.encomendas = lista.stream().map(Encomenda :: clone).collect(Collectors.toList());
    }

    public void addEnc(Encomenda enc){
        if(!this.encomendas.contains(enc)) {
            this.encomendas.add(enc.clone());
        }
    }

    public void updateClass(double rating){
        this.nclass += 1;
        this.classificacao += rating;
        setNclass(this.nclass);
        setClassificacao(this.classificacao / this.nclass);
    }

    /**
     * Método que faz uma cópia da classe Empresaentrega.
     * Para tal invoca o construtor de cópia.
     * @param
     * @return User clone da classe Empresaentrega
     */
    public Empresaentrega clone(){
        return new Empresaentrega(this);
    }

   /**
     * Método que devolve a representação em String da classe Empresaentrega.
     * @param
     * @return String
     */
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("Empresa de entrega ---> ");
        sb.append("\tNome: ").append(super.getNome()).append("\n");
        sb.append("\tEmail: ").append(super.getUsername()).append("\n");
        sb.append("\tPosicao: ").append(super.getPosicao());
        sb.append("\tNIF: ").append(this.getNif()).append("\n");
        sb.append("\tPronta a receber: ").append(this.getProntaReceber()).append("\n");
        sb.append("\tTaxa: ").append(this.getTaxa()).append("\n");
        sb.append("\tCapacidade: ").append(this.getCapacidade()).append("\n");
        sb.append("\tVelocidade: ").append(this.getVelocidade()).append("\n");
        sb.append("\tClassificacao: ").append(this.getClassificacao()).append("\n");
        sb.append("\tNumero de Classificaçoes: ").append(this.getNclass()).append("\n");
        sb.append("\tRaio: ").append(this.getRaio()).append("\n");
        sb.append("\tVisto Medico: ").append(this.getVistoMedico()).append("\n");
        sb.append("\tLista de encomendas: ").append(this.encomendas.toString()).append("\n");
        return sb.toString();
    }

     /**
     * Método que verifica se um Object é igual à classe Empresaentrega atual.
     * @param o
     * @return boolean
     */
      public boolean equals(Object o){
        if (o==this)return true;
        if ((o==null) || o.getClass()!=this.getClass())return false;

        Empresaentrega emp = (Empresaentrega) o;

       return (emp.getUsername().equals(this.getUsername()));
    }

   public int hashCode(){
          long aux1, aux2,aux3,aux4;
          int hash = 5;
          hash = 31 * hash + super.hashCode();
          hash = 31 * hash + this.nif.hashCode();
          hash = 31 * hash + (this.prontaReceber ? 1 : 0);
          aux1 = 31 * hash + Double.doubleToLongBits(this.taxa);
          hash = 31 * hash + (int)(aux1 ^ (aux1 >>> 32));
          hash = 31 * hash + Integer.hashCode(this.capacidade);
          aux2 = 31 * hash + Double.doubleToLongBits(this.velocidade);
          hash = 31 * hash + (int)(aux2 ^ (aux2 >>> 32));
          aux3 = 31 * hash + Double.doubleToLongBits(this.classificacao);
          hash = 31 * hash + (int)(aux3 ^ (aux3 >>> 32));
          hash = 31 * hash + Integer.hashCode(this.nclass);
          aux4 = 31 * hash + Double.doubleToLongBits(this.raio);
          hash = 31 * hash + (int)(aux4 ^ (aux4 >>> 32));
          hash = 31 * hash + (this.vistoMedico ? 1 : 0);
          hash = 31 * hash + this.encomendas.stream().mapToInt(Encomenda::hashCode).sum();
          return hash;
   }

}