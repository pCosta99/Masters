public class Transportadoras
{
    private int nif;
    private String codempresa,nome;
    private double x,y,raio,precokm,kmpq;
    private boolean disp,med;//disponiblidade medicamentos
    private float c;//clasiificação
    private int n;
    
    public Transportadoras(){
       this.nome=""; 
       this.codempresa="";
       this.x=0;
       this.y=0;
       this.raio=0;
       this.precokm=0;
       this.disp=true;
       this.med=false;
       this.c=0;
       this.n=0;
       this.kmpq=0;
    }
       
   public Transportadoras(String name,String code,double latitude ,double longitude,double ray ,double preco,float c, int n,double km,boolean dis,boolean medic){
       this.nome=name; 
       this.codempresa=code;
       this.x=latitude;
       this.y=longitude;
       this.raio=ray;
       this.precokm=preco;
       this.disp=dis;
       this.med=medic;
       this.c=c;
       this.n=n;
       this.kmpq=km;
    }
    
    //copy
    public Transportadoras(Transportadoras t) {
       this.nome=t.getNome(); 
       this.codempresa=t.getCodEmp();
       this.x=t.getX();
       this.y=t.getY();
       this.raio=t.getRaio();
       this.precokm=t.getPrecoKm();
       this.disp=t.getDisp();
       this.med=t.getMed();
       this.c=t.getC();
       this.n=t.getN();
       this.kmpq=t.getKMPQ();
      }
      
      //retorna variaveis dentro da classe
      
    public String getNome() {
        return this.nome;
      }
      
    public String getCodEmp() {
        return this.codempresa;
      }
    public double getKMPQ() {
        return this.kmpq;
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
      
    public double getPrecoKm(){
        return this.precokm;
        }
      
    public boolean getDisp(){
        return this.disp;
        }
        
    public boolean getMed(){
        return this.med;
        }  
    
    public int getN(){
          return this.n;
        }
      
      public float getC(){
        return this.c;
      }     
      //recebe uma variavel e poe na classe  
    public void setNome(String novoNome) {
          this.nome = novoNome;
        }
        
    public void setEmp(String novoCodE) {
          this.codempresa = novoCodE;
        }
    public void setKMPQ(double novokm) {
          this.kmpq = novokm;
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
        
    public void setPrecoKm(double novopkm){
         this.precokm=novopkm;
        }
        
    public void setDisp(boolean novodisp){
        this.disp=novodisp;
        }
        
    public void setMed(boolean novomed){
        this.med=novomed;
        }
    
    public void setC(float novoC) {
          float i;
          i=(this.c)*(this.n);
          this.n+=1;
          this.c=(i+novoC)/n;
        }    
        
    public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        Transportadoras umTransportadoras = (Transportadoras) o;
        return (this.codempresa.equals(umTransportadoras.getCodEmp()));
    } 
        
    public String toString() {
            StringBuilder sb = new StringBuilder();
              sb.append("Transportadora:");
              sb.append(this.codempresa);
              sb.append(","+this.nome); 
              sb.append(","+this.x);
              sb.append(","+this.y);
              sb.append(","+this.raio);
              sb.append("," +this.precokm); 
              sb.append("," +this.disp); 
              sb.append("," +this.med); 
              sb.append("," +this.c);
              sb.append("," +this.n);
              sb.append("," +this.kmpq);
            return sb.toString();
    }
         
    public Transportadoras clone(){
        return new Transportadoras(this);
        }
    

    public double precoencomenda(double utx,double uty,double lox,double loy,double peso){
       double pi = 3.14159265358979323846;
utx = utx * pi / 180;
uty = uty * pi / 180;
lox = lox * pi / 180;
loy =loy* pi/ 180;
return  this.precokm*Math.acos( Math.cos( utx ) * Math.cos( lox ) * Math.cos( loy - uty ) +Math.sin( utx ) * Math.sin( lox ) )+peso;
    }
      
      public boolean dentroRaio(double lox,double loy){
        //devolve true sea loja não estiver no raio da voluntario
        double pi = 3.14159265358979323846;
        double distlo=Math.acos( Math.cos( this.x*pi/180 ) * Math.cos( lox* pi / 180 ) * Math.cos( loy - this.y*pi/180 ) +Math.sin( this.x*pi/180 ) * Math.sin( lox * pi / 180) );
       
        //loja<-->voluntario
        
        if (this.raio > distlo) {
            return true;
        }
        return false;
    }
    public boolean dentroRaio2(double uox,double uoy){
        //devolve true sea loja não estiver no raio da voluntario
        double pi = 3.14159265358979323846;
        double distlo=Math.acos( Math.cos( this.x*pi/180 ) * Math.cos( uox* pi / 180 ) * Math.cos( uoy - this.y*pi/180 ) +Math.sin( this.x*pi/180 ) * Math.sin( uox * pi / 180) );
       
       
        //utilizador<-->voluntario
        
        if (this.raio > distlo) {
            return true;
        }
        return false;
    }
}
