
public class Coordenadas {
  private double x;
  private double y;
  private UmVoluntario vols;
  private UmaLoja loja;
 


  public Coordenadas() {
    this.x = 0;
    this.y = 0;
  }
  public Coordenadas getcoordenadasvol(){
  return vols.getgps();    
  }
 
  public Coordenadas getcoordenadasloja(){
  return loja.getgps();    
  }
  
  public Coordenadas(double cx, double cy) {
    this.x = cx;
    this.y = cy;
  }
  
  
  
  
  public Coordenadas(Coordenadas umPonto) {
    this.x = umPonto.getX();
    this.y = umPonto.getY();
  }

  

  public double getX() {
    return this.x;
  }
  

  public double getY() {
    return this.y;
  }
  

  public void setX(int novoX) {
    this.x = novoX;
  }
  

  public void setY(int novoY) {
    this.y = novoY;
  }
  public void movePonto(int cx, int cy) {
    this.x = cx;  
    this.y = cy;  
  }
  

  public boolean ePositivo() {
    return (this.x > 0 && this.y > 0);
  }
  
 
  public double distancia(Coordenadas umPonto) {
      
    return Math.sqrt(Math.pow(this.x - umPonto.getX(), 2) +
                     Math.pow(this.y - umPonto.getY(), 2));
  }
  
  
     public double distanciadoispontos(Coordenadas a, Coordenadas b){
     double x = a.getX();
     double xx = b.getX();
     double y = a.getY();
     double yy = b.getY();
     double distancia = 0.0;
     if(x>xx && y>yy){
     
     distancia= Math.sqrt((x - xx) * 2 + (y-yy)*2); 
     return distancia;}
     
     if(xx>x && yy>y){
     distancia= Math.sqrt((xx - x) * 2 + (yy-y)*2); 
     return distancia;}
        
     if(x>xx && yy>y){
     distancia= Math.sqrt((x - xx) * 2 + (yy-y)*2); 
     return distancia;}

     if(x>xx && y>yy){
     distancia= Math.sqrt((x - xx) * 2 + (y-yy)*2); 
     return distancia;}
     
     if(x==xx && y>yy){
     distancia= Math.sqrt((y-yy)*2); 
     return distancia;}
     
     if(x>xx && y==yy){
     distancia= Math.sqrt((x - xx) * 2 ); 
     return distancia;}
     
     if(x==xx && yy>y){
     distancia= Math.sqrt((yy-y)*2); 
     return distancia;}
     
     if(xx>x && y==yy){
     distancia= Math.sqrt((xx - x) * 2 ); 
     return distancia;}
     
     return distancia; 
     }

    
  public boolean iguais(Coordenadas umPonto) {
    return (this.x == umPonto.getX() && this.y == umPonto.getY());
  }   
  
  

  private boolean xIgualAy() {
    return (Math.abs(this.x) == Math.abs(this.y));
  }
  
 
  public String toString() {
    return "Latitude: " + this.x + "\nLongitude:" + this.y;
  }
  
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if ((o == null) || (this.getClass() != o.getClass()))
      return false;
    Coordenadas p = (Coordenadas) o;
    return (this.x == p.getX() && this.y == p.getY());
      
  }
 
  public Coordenadas clone() {
    return new Coordenadas(this);    
  }    
  
  public double distanciadoispontoss(Coordenadas a,UmaLoja b){
  Coordenadas c = b.getgps(); 
  return distanciadoispontos(a,c);
  }
  
 
}