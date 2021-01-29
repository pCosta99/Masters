
public class Coordenadas {
  private double x;
  private double y;
  private Voluntario vols;
  private Loja loja;
 
  //construtores

  public Coordenadas() {
    this.x = 0;
    this.y = 0;
  }
  
  public Coordenadas getcoordenadasvoluntario(){
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
  
  
  public double distancia2pontos(Coordenadas a, Coordenadas b){
  double distancia= Math.sqrt((a.getX() - b.getX()) * 2 + (a.getY()-b.getY())*2); 
  return distancia;
  }
  
  //género de equals
  public boolean iguais(Coordenadas umPonto) {
    return (this.x == umPonto.getX() && this.y == umPonto.getY());
  }   
  
  

  private boolean xIgualAy() {
    return (Math.abs(this.x) == Math.abs(this.y));
  }
  
 
  public String toString() {
    return "Ponto: --> x = " + this.x + " -  y = " + this.y;
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
  
  public double distancia2pontoss(Coordenadas a,Loja b){
  Coordenadas c = b.getgps(); 
  return distancia2pontos(a,c);
  }
  
 
}