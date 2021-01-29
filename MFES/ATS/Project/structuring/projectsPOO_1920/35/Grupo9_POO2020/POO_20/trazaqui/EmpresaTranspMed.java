import java.util.*;
/**
 * Escreva a descrição da classe EmpresaTranspMed aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class EmpresaTranspMed extends EmpresaTransp implements Medicamentos
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private boolean aceitaMed;

    /**
     * COnstrutor para objetos da classe EmpresaTranspMed
     */
    public EmpresaTranspMed()
    {
       super();
       this.aceitaMed = false;
    }
    
 

    public EmpresaTranspMed(String id, String nome, Coordenadas coordenadas, 
   String email, String password, String morada, double classificacao, double raio, ArrayList<Avaliacao> avaliacoes,
   ArrayList<Encomenda> historico, Integer nifE, double kms,
   double precokm,boolean Med)
    {
        super(id, nome, coordenadas, email, password, morada, classificacao, raio, avaliacoes, historico,
       nifE,kms,precokm);
        this.aceitaMed = false;
    }
   public EmpresaTranspMed(EmpresaTranspMed emp)
    {
        super(emp);
        this.aceitaMed = emp.aceitoTransporteMedicamentos();
    
    }
   public boolean aceitoTransporteMedicamentos(){
       return this.aceitaMed;
   }
   
   public void aceitaMedicamentos(boolean state){
       this.aceitaMed = !this.aceitaMed;
   }
   
   
   
   
   /**
    * Metodo Equals
    */
   public boolean equals(Object o){
       if (this == o)
           return true;
           
       if (o == null || this.getClass() != o.getClass())
            return false;
            
       EmpresaTranspMed emp = (EmpresaTranspMed) o;
       
       return super.equals(emp);
   }
   
   //Clone
    
   public EmpresaTranspMed clone(){
       return new EmpresaTranspMed (this);
   }
    
    
}
