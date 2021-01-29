import java.io.Serializable;
class GPS implements Serializable{
  
  double x;
  double y;
  
  public GPS (String gps) {
      
      String tokens[] = gps.split(",");
      
      double x = 0;
      double y = 0;
      try{
          x = Double.parseDouble(tokens[0]);
        }
      catch (NumberFormatException e){
          System.out.println(e.getMessage());
        }
        
      try{
          y = Double.parseDouble(tokens[1]);
      }
      catch (NumberFormatException e){
          System.out.println(e.getMessage());
        } 
      catch (ArrayIndexOutOfBoundsException e){
          System.out.println(e.getMessage());
        }
      
      this.x = x;
      this.y = y;
  }
  
    public GPS (double num1, double num2){
      this.x = num1;
      this.y = num2;  
  }
  
  public double p1(){
      return this.x;
  }
    
  public double p2()
  {
       return this.y;
  }
  
  public double getX ()
  {
      return this.x;
  }
    
  public double getY(){
      return this.y;
  }
  
  public double dist(GPS outro){
      double DLA,DLO;
      DLA = (this.x - outro.getX())/60;
      DLO = (this.y - outro.getY())/60;
      return Math.sqrt(Math.pow(DLA,2) + Math.pow(DLO, 2));  
  }
  
  public boolean inRaio(double raio, GPS o, GPS d){
      return this.dist(o) <= raio && this.dist(d) <= raio;
  }
}