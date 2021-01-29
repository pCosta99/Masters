import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class EmpresaTransp extends Transportador implements Serializable{
    
   //Variaveis de instância
   private Integer nifE; //nif da transportadora
   private double kms; //nº de kms realizados 
   private double precokm; //preco por km

   /**
    * Construtor vazio
    */
   public EmpresaTransp(){
        super("Empresa Transportadora","",null,"","","",0,0,null,null);
        this.nifE = 0;
        this.kms = 0;
        this.precokm = 0;
   }

   /**
    * Construtor por parametrizado
    */
   public EmpresaTransp(String id, String nome, Coordenadas coordenadas, 
   String email, String password, String morada, double classificacao, double raio, ArrayList<Avaliacao> avaliacoes,
   ArrayList<Encomenda> historico,  Integer nifE, double kms,
   double precokm){
       super(id, nome, coordenadas, email, password, morada, classificacao, raio, avaliacoes, historico);
       this.nifE = nifE;
       this.kms = kms;
       this.precokm = precokm;
   }
   
   /**
    * Construtor por cópia
    */
   public EmpresaTransp(EmpresaTransp e) {
        super(e);
        this.nifE = e.getNifE();
        this.kms = e.getKms();
        this.precokm = e.getPrecoKm();
   }

   //Getters
   
   public Integer getNifE(){
       return this.nifE;
   }
   
   public double getKms(){
       return this.kms;
   }
   
   public double getPrecoKm(){
       return this.precokm;
   }
   
   //Setters
   
   public void setNifE(Integer n){
       this.nifE = n;
   }

   public void setKms(double k){
       this.kms = k;
   }
    
   public void setPrecoKm(double pk){
       this.precokm = pk;
   }
    
   /**
    * Metodo Equals
    */
   public boolean equals(Object o){
       if (this == o)
           return true;
           
       if (o == null || this.getClass() != o.getClass())
            return false;
            
       EmpresaTransp e = (EmpresaTransp) o;
       
       return super.equals(e) &&
       this.nifE == e.getNifE() &&
       this.kms == e.getKms() &&
       this.precokm == e.getPrecoKm();
   }

   public String toString(){
       StringBuilder sb = new StringBuilder(); 
       sb.append("NIF: " + this.getNifE() + "\n");
       sb.append("Nº de kilometros percorridos: " + this.getKms() + "\n");
       sb.append("Preço por kilometro: " +this.getPrecoKm() + "\n");  
       return sb.toString();
   }
   
   //Clone
    
   public EmpresaTransp clone(){
       return new EmpresaTransp(this);
   }
   
}