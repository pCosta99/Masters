package Model;

/**
 * Classe que regista o custo e o tempo associado a cada Empresa
 */

public class custoTempo {
    private double custo;
    private double tempo;

    public custoTempo(){
        this.custo = 0.0;
        this.tempo = 0.0;
    }

    public custoTempo(double custo,double tempo){
        this.custo = custo;
        this.tempo = tempo;
    }

    public custoTempo(custoTempo ct){
        this.custo = ct.getCusto();
        this.tempo = ct.getTempo();
    }


/**
 * Getter do custo da empresa
 */

   public double getCusto(){
       return this.custo;
   }
   

/**
 * Getter do tempo da empresa
 */

   public double getTempo(){
       return this.tempo;
   }
   
/**
 * Setter do custo da empresa
 */

   public void setCusto(double c){
       this.custo = c;
   }

/**
 * Setter do tempo da empresa
 */
   public void setTempo(double t){
       this.tempo = t;
   }

   

}