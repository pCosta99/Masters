import java.text.DecimalFormat;
/**
 * classe que representa os Voluntarios 
 */
/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Voluntario extends User{
    
    private boolean disponivel;
    /** Coordenadas Gps */
    private Localizacao posicao;
    /** Raio de entrega */
    private double raio;
    
    private double velocidade; 
    
    private Transporte transporte;
    private double classificacao;
    
   /**
     * Construtor sem argumentos
     */ 
   public Voluntario(){
        posicao = new Localizacao();
        raio = 0.0;
        velocidade = 0.0;
        disponivel = false;
        classificacao = 0;
   }
   
   /** Construtor por parametros 
    * @param email: email do voluntario
    * @param nome: nome do voluntario
    * @param password: password do voluntario
    * @param raio: raio onde o voluntario se movimenta
    * @param velocidade: velocidade a que o voluntario se desloca
    * @param posiçao: local onde o voluntario se encontra 
    * @param classificaçao: classificaçao do voluntario
    */
   public Voluntario(String email,String nome, String password, double raio,double velocidade, boolean disponivel,Localizacao loc){
        super(email,nome,password);
        this.raio=raio;
        this.disponivel=disponivel;
        this.posicao = loc;
        this.velocidade = velocidade;
        this.classificacao= 0;
   }
   
   /**
     * Construtor de copia do objeto Voluntario
     */
   public Voluntario(Voluntario v){
        super(v);
        this.raio = v.getRaio();
        this.disponivel = v.getDisponivel();
        this.posicao = v.getPosicao();
        this.classificacao=v.getClassificacao();
   }
    
   /**
     * Get da classificaçao do Voluntario
   */
   public double getClassificacao(){
       return classificacao;
   }
   
   /**
     * Get do raio onde o Voluntario se movimenta
   */ 
   public double getRaio(){
       return raio;
   }
   
   /**
     * Get da Disponibilidade 
   */ 
   public boolean getDisponivel(){
       return disponivel;
   }
   
   /**
     * Get da Localizaçao
   */ 
   public Localizacao getPosicao(){
       return posicao;
   }  
   
    /**
     * Get da Velocidade
   */
   public double getVelocidade(){
       return velocidade;
   } 
   
   /**
     * Altera o raio para onde se desloca
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
     * Altera a posiçao 
   */
   public  void setPosicao (Localizacao posicao){
       this.posicao = posicao;
   } 
   
   /**
     * Altera a velocidade 
   */
   public void setVelocidade(double velocidade){
       this.velocidade = velocidade;
   }
   
   /**
     * Altera a velocidade 
   */
    public void setClassificacao(double classificacao){
        this.classificacao = classificacao;
    }
   
    /**
     * Método equals
     * compara um objeto para ver se é Voluntario
     */
   public boolean equals(Object o){
        if( o == this ) return true;
        if( o == null || o.getClass() != this.getClass()) return false;
        Voluntario d = (Voluntario) o;
        
        return super.equals(o) && raio == d.getRaio() &&
                    disponivel == d.getDisponivel() && 
                    posicao.equals( d.getPosicao()) &&
                    velocidade == d.getVelocidade() &&
                    classificacao == d.getClassificacao()
                    ;
    } 
    
   /**
     * Metodo clone faz uma copia do objecto Voluntario
     * @return uma cópia do User
     */ 
   public Voluntario clone(){   
        return new Voluntario(this);
   }
   
   /**
     * Implementação do método toString
     * @return uma string com a informação de um Voluntario
     */
   public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append( super.toString());
        
        sb.append("Posição:");
        sb.append(this.posicao + "\n");
        sb.append("Raio de entrega: " );
        sb.append(this.raio + "\n");
        sb.append("Disponibilidade: ");
        sb.append(this.disponivel);
        sb.append("Velocidade: ");
        sb.append(this.velocidade+"\n");
        sb.append("Classificação: ");
        sb.append(this.classificacao);
        
        return sb.toString();
    }
   
    
    /**
    * Diz se está disponivel para fazer entrega
    */
   public boolean estaDisponivel(){
        return this.disponivel;
    } 
   
    DecimalFormat df = new DecimalFormat("#.0");
   /**
    * Calcula uma estimativa do tempo
    */ 
   public void fazEstimativa(Transporte transporte, DataBase db){
       double kms = transporte.getDistUtilizador();
       String codLoja = transporte.getLoja();
       Loja l = (Loja) db.getUser(codLoja);
       int pessoasEspera = l.getPessoasEspera(); //pessoas em espera
       
       double tempoEst = (kms*60)/getVelocidade() + pessoasEspera*5;
       System.out.println("Tempo estimado de transporte: "+df.format(tempoEst)+" minutos"+"\n");
       transporte.setTempoEstimado((kms*60)/getVelocidade() + pessoasEspera*5);
   } 
   
   
   public void addTransporte(int index, DataBase db){
       super.addTransporte(index);
       //incompleto
    }
}
