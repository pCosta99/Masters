
/**
 * Escreva a descrição da classe GPS aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.io.Serializable;
public class GPS implements Serializable
{    
  //variÃ¡veis de instÃ¢ncia
  private double x;
  private double y;
  
  /**
   * Construtor por omissão de GPS.
   */
  public GPS() {
    this.x = 0;
    this.y = 0;
  }
  
  /**
   * Construtor parametrizado de GPS.
   */
  public GPS(double cx, double cy) {
    this.x = cx;
    this.y = cy;
  }
  
  /**
   * Construtor de cópia de GPS.
   */
  public GPS(GPS umGPS) {
    this.x = umGPS.getX();
    this.y = umGPS.getY();
  }
  
  //getters
  public double getX() {
    return this.x;
  }
  
  public double getY() {
    return this.y;
  }
  
  //setters
  public void setX(double novoX) {
    this.x = novoX;
  }
  
  public void setY(double novoY) {
    this.y = novoY;
  }
  
  /**
   * método que calcula distancia entre duas localizações
   */
  public double distancia(GPS l1,GPS l2) {
      return Math.sqrt(Math.pow(l1.getX() - l2.getX(), 2) + Math.pow(l1.getY() - l2.getY(), 2));
  }

  /**
   * Método que devolve a representaçãoo em String do GPS.
   */
  public String toString() {
    return this.x + "," + this.y;
  }
  
  /**
   * Método que vê se dois GPS são iguais.
   */
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if ((o == null) || (this.getClass() != o.getClass()))
      return false;
    GPS p = (GPS) o;
    return (this.x == p.getX() && this.y == p.getY());
      
  }
  
  /**
   * Método que faz uma cópia do objecto receptor da mensagem.
   */
  public GPS clone() {
    return new GPS(this);    
  }    
}
