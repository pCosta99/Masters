
/**
 * Escreva a descrição da classe Voluntarios aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Voluntarios
{
    private String nome,codvoluntario;
    private double x,y,raio;
    private boolean disp;
    private float c;//clasiificação
    private int n;//num de classificaçoes
   
    private boolean med;
    public Voluntarios(){
       this.nome=""; 
       this.codvoluntario="";
       this.x=0;
       this.y=0;
       this.raio=0;
       this.disp=false;
       this.c=0;
       this.n=0;
       
       this.med=false;
    }
       
   public Voluntarios(String name,String codv,double latitude ,double longitude,double ray,boolean disp,float c, int n,boolean med){
       this.nome=name; 
       this.codvoluntario=codv;
       this.x=latitude;
       this.y=longitude;
       this.raio=ray;
       this.disp=disp;
       this.c=c;
       this.n=n;
       
       this.med=med;
    }
    
    //copy
    public Voluntarios(Voluntarios vol) {
       this.nome=vol.getNome(); 
       this.codvoluntario=vol.getCodVol();
       this.x=vol.getX();
       this.y=vol.getY();
       this.raio=vol.getRaio();
       this.disp=vol.getDisp();
       this.c=vol.getC();
       this.n=vol.getN();
       
       this.med=vol.getMed();
      }
      
      //retorna variaveis dentro da classe
      public String getNome() {
        return this.nome;
      }
      public String getCodVol() {
        return this.codvoluntario;
      }
      public double getX() {
        return this.x;
      }
      public double getY() {
        return this.y;
      }
      
      public double getRaio() {
        return this.raio;
      }
      public boolean getDisp() {
        return this.disp;
      }
      public int getN(){
          return this.n;
        }
      
      public float getC(){
        return this.c;
      }
      
      public boolean getMed() {
        return this.med;
      }
      //recebe uma variavel e poe na classe
     
        public void setNome(String novoNome) {
          this.nome = novoNome;
        }
        public void setVol(String novoCodV) {
          this.codvoluntario = novoCodV;
        }
        
        public void setX(double novoX) {
          this.x = novoX;
        }
        public void setY(double novoY) {
          this.y = novoY;
        }
        
        public void setRaio(double novoRaio) {
          this.raio = novoRaio;
        }
        public void setDisp(boolean novoD) {
          this.disp = novoD;
        }
        public void setC(float novoC) {
          float i;
          i=(this.c)*(this.n);
          this.n+=1;
          this.c=(i+novoC)/n;
        }
        
        public void setMed(boolean novoM) {
          this.med = novoM;
        }
        public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        Voluntarios umVoluntario = (Voluntarios) o;
        return (this.codvoluntario.equals(umVoluntario.getCodVol()));
    } 
        
        public String toString() {
            StringBuilder sb = new StringBuilder();
      sb.append("Voluntario:");
      sb.append(this.codvoluntario);
      sb.append(","+this.nome); 
      sb.append(","+this.x);
      sb.append(","+this.y);
      sb.append(","+this.raio);
      sb.append(","+this.disp);
      sb.append(","+this.c);
      sb.append(","+this.n);
     
      sb.append(","+this.med);
      return sb.toString();
         } 
         
        public Voluntarios clone(){
        return new Voluntarios(this);
        }
        
         public boolean dentroRaio(double lox,double loy){
        //devolve true sea loja não estiver no raio da voluntario
        double distlo;
       
        distlo=Math.sqrt(Math.pow(this.x-lox,2)+Math.pow(this.y-loy,2));//loja<-->voluntario
        
        if (this.raio > distlo) {
            return true;
        }
        return false;
    }
    public boolean dentroRaio2(double uox,double uoy){
        //devolve true sea loja não estiver no raio da voluntario
        double distlo;
       
        distlo=Math.sqrt(Math.pow(this.x-uox,2)+Math.pow(this.y-uoy,2));//utilizador<-->voluntario
        
        if (this.raio > distlo) {
            return true;
        }
        return false;
    }
    
}
