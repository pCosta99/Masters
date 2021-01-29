import java.util.*;
/**
 * Escreva a descrição da classe VoluntarioMed aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class VoluntarioMed extends Voluntario implements Medicamentos
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private boolean aceitaMed;

    /**
     * COnstrutor para objetos da classe VoluntarioMed
     */
    public VoluntarioMed()
    {
        super();
        this.aceitaMed = false;
    }

    public VoluntarioMed(String id, String nome, Coordenadas coordenadas, 
   String email, String password, String morada, double classificacao, double raio, ArrayList<Avaliacao> avaliacoes,
   ArrayList<Encomenda> historico,boolean Med)
    {
        super(id, nome, coordenadas, email, password, morada, classificacao, raio, avaliacoes, historico);
        this.aceitaMed = false;
    }
   public VoluntarioMed(VoluntarioMed v) 
    {
        super(v);
        this.aceitaMed = v.aceitoTransporteMedicamentos();
    
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
            
       VoluntarioMed v = (VoluntarioMed) o;
       
       return super.equals(v);
   }
   
   //Clone
    
   public VoluntarioMed clone(){
       return new VoluntarioMed(this);
   }
    
}
