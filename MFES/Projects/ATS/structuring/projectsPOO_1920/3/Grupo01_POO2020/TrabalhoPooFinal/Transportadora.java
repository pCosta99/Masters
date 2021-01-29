import java.util.stream.Collectors;
import java.text.DecimalFormat;
/**
 * classe que representa a transportadora 
 */
/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Transportadora  extends User{

    private Localizacao posicao;
    /** Raio de entrega */
    private double raio;
    private int nif;
    /** Preço por kms*/
    private double precoKm;//preço por km 
    
    private double kmsTotal;
    private double totalFacturado;
    
    private boolean disponivel;
    private double classificacao;
    
    private double velocidade;//velocidade do carro por a pedir no menu
    /** Associa um transporte à transportadora*/
    
    private Transporte transport;
    
    /**
     * Construtor sem argumentos
     */
    public Transportadora(){
        super(); 
        this.raio=0.0;
        this.nif=0;
        this.disponivel=false;
        this.precoKm=0.0;
        this.velocidade=0.0;
        this.totalFacturado=0.0;
        this.kmsTotal=0;
        this.classificacao=0;
        posicao = new Localizacao();
   }
   
   /**
     * Construtor por paramentros
     * @param email: email da Transportadora
     * @param nome: nome da Transportadora
     * @param password: password da Transportadora
     * @param loc: localizaçao da Transportadora
     * @param raio: raio de acçao da Transportadora
     * @param preco: valor da Transportadora 
     * @param nif: NIF da Transportadora 
     * @param disponivel: disponibilidade da Transportador
     */
   public Transportadora(String email,String nome, String password, double raio, double preco,int nif, boolean disponivel,
                  Localizacao loc){
        super(email,nome,password);
 
        this.raio=raio;
        this.disponivel=disponivel;
        this.nif=nif;
        this.precoKm=preco;
        this.velocidade=0;
        this.kmsTotal=0;
        this.totalFacturado=0;
        this.classificacao=0;
        this.posicao = loc;
   }
   
   /**
     * Construtor de copia do objeto Transportadora 
     */
   public Transportadora(Transportadora t){
        super(t);
     
        this.raio = t.getRaio();
        this.disponivel = t.getDisponivel();
        this.nif=t.getNif();
        this.precoKm = t.getPrecoKm();
        this.velocidade=t.getVelocidade();
        this.kmsTotal=t.getKmsTotal();
        this.totalFacturado=t.getTotalFacturado();
        this.posicao = t.getPosicao();
        this.classificacao = t.getClassificacao();
   }
    
   /**
     * Get do Total Faturado
     */
   public double getTotalFacturado(){
       return totalFacturado;
   }
   
   /**
     * Get da classificaçao 
   */
   public double getClassificacao(){
       return classificacao;
   }
   
   /**
     * Get do raio
   */
   public double getRaio(){
       return raio;
   }
   
   /**
     * Get do Disponibilidade 
   */
   public boolean getDisponivel(){
       return disponivel;
   }
   
   /**
     * Get do NIF 
   */
   public int getNif(){
       return nif;
    }
    
   /**
     * Get da Velocidade 
   */ 
   public double getVelocidade(){
       return velocidade;
    } 
    
   /**
     * Get do Preço por Km
   */  
   public double getPrecoKm(){
       return precoKm;
   } 
   
   /**
     * Get dos Kms Total
   */  
   public double getKmsTotal(){
       return kmsTotal;
   }
   
   /**
     * Get da Localizaçao
   */ 
   public Localizacao getPosicao(){
       return posicao;
   } 
   
   /**
     * Altera o total faturado
     */
   public void setTotalFacturado(double totalFacturado){
       this.totalFacturado=totalFacturado;
   }
   
   /**
     * Altera o raio
   */
   public void setRaio (double raio){
       this.raio = raio;
   }
   
   /**
     * Altera a disponibilidade 
   */
   public void setDisponivel (boolean disponivel){
       this.disponivel = disponivel;
    }
    
   /**
     * Altera os kms totais 
   */ 
   public void setKmsTotal(double kmsTotal){
       this.kmsTotal = kmsTotal;
   }
   
   /**
     * Altera o NIF 
   */ 
   public void setNif(int nif){
       this.nif=nif;
   }
   
   /**
     * Altera a velocidade 
   */ 
   public void setVelocidade(double velocidade){
       this.velocidade=velocidade;
   } 
   
   /**
     * Altera o preço por km 
   */ 
   public void setPrecoKm(double precoKm){
       this.precoKm = precoKm;
   }
   
   /**
     * Altera a posiciçao 
   */ 
   public  void setPosicao (Localizacao posicao){
       this.posicao = posicao;
   }
   
   /**
     * Altera a classificaçao 
   */ 
   public void setClassificacao(double classificacao){
       this.classificacao=classificacao;
    }
    
   /**
     * Método equals
     * compara um objeto para ver se é Transportadora 
    */
   public boolean equals(Object o){
        if( o == this ) return true;
        if( o == null || o.getClass() != this.getClass()) return false;
        Transportadora d = (Transportadora) o;
        
        return super.equals(o) && 
               raio == d.getRaio() &&disponivel == d.getDisponivel() && kmsTotal==d.getKmsTotal()&& nif==d.getNif()
               &&precoKm == d.getPrecoKm()&&velocidade==d.getVelocidade() && totalFacturado==d.getTotalFacturado()
               && classificacao==d.getClassificacao()
               && posicao.equals( d.getPosicao() );
    }
    
   /**
     * Metodo clone faz uma copia do objecto Transportadora 
     * @return uma cópia do User
     */
   public Transportadora clone(){   
        return new Transportadora(this);
   }
   
   /**
     * Implementação do método toString
     * @return uma string com a informação de um User
     */
   public String toString(){
        DecimalFormat df = new DecimalFormat("#.0");
        StringBuilder sb = new StringBuilder();
        
        sb.append( super.toString());
        
   
        sb.append("\nPosição:");
        sb.append(this.posicao + "\n");
        sb.append("NIF:");
        sb.append(this.nif+"\n");
        sb.append("Raio de entrega: " );
        sb.append(this.raio + "\n");
        sb.append("Preço por km: ");
        sb.append(this.precoKm + "\n");
        sb.append("Velocidade:");
        sb.append(this.velocidade+"\n");
        sb.append("Classificação: ");
        sb.append(this.classificacao+"\n" );
        sb.append("Total de kms: ");
        sb.append(this.df.format(kmsTotal)+"\n");
        sb.append("Disponibilidade: ");
        sb.append(this.disponivel+"\n");
     
        return sb.toString();
    }
   
   /**
    * Diz se a transportadora está disponivel para fazer entrega
    */
   public boolean estaDisponivel(){
        return this.disponivel;
    } 
    
    DecimalFormat df = new DecimalFormat("#.0");
    /**
    * Faz um orçamento 
    */
    public void fazOrçamento(Transporte transporte, DataBase db){
       double kms = transporte.getDistUtilizador();
       String email = transporte.getLoja();
       Loja l = (Loja) db.getUser(email);
       int pessoasEspera = l.getPessoasEspera(); //pessoas em espera
       System.out.println("-Pessoas em espera: "+pessoasEspera+"; -Velocidade da tansportadora: "+getVelocidade()+"; -Distancia da loja:"+df.format(kms)+ ";\n");
       
       double tempoEst = (kms*60)/getVelocidade() + pessoasEspera*5;
       double precoEst = kms*getPrecoKm();
       System.out.println("Tempo estimado de transporte: "+df.format(tempoEst)+" minutos"+"\n");
       System.out.println("Preço estimado de transporte: "+df.format(precoEst)+" euros"+"\n");
    }
   
    /**
    * Quando o utilizador aceita o orçamento do transporte
    */
   public void aceitaOrçamento(Transporte transporte, DataBase db){
       double kms = transporte.getDistUtilizador();
       String email = transporte.getLoja();
       Loja l = (Loja) db.getUser(email);
       int pessoasEspera = l.getPessoasEspera(); //pessoas em espera
       
       double tempoEst = (kms*60)/getVelocidade() + pessoasEspera*5;
       double precoEst = kms*getPrecoKm();
       
       transporte.setTempoEstimado((kms*60)/getVelocidade() + pessoasEspera*5);
       transporte.setPrecoEstimado((kms*getPrecoKm()));
    
   }

   // depois de fazer um transporte adiciona os dados desse transporte à transportadora
   public void addTransporte(int index, DataBase db){
       super.addTransporte(index);
       Transporte t = db.getTransporte(index);
       kmsTotal += t.getDistUtilizador();
       totalFacturado += t.getPrecoReal();
    }
}
