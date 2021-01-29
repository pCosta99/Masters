import static java.lang.Math.abs;
import java.lang.Math; 
import java.util.*;
import java.io.*;


public class Localizacao implements Serializable {

    //VARI�?VEIS DE INSTANCIA
    private double latitude, longitude;

    //CONSTRUTORES USUAIS
    public Localizacao (double cx, double cy) { latitude = cx; longitude = cy; }

    public Localizacao () {
        this(0.0, 0.0); }

    public Localizacao (Localizacao l) { 
        latitude = l.getX();
        longitude = l.getY(); }

    //MÉTODOS DA INSTÂNCIA
    public double getX() { return latitude; } 

    public double getY() { return longitude; }

    //INCREMENTOS DAS COORDENADAS
    public void incCoord (double dx, double dy) {
        latitude += dx;
        longitude += dy;
    } 

    //DECREMENTO DAS COORDENADAS
    public void decCoord (double dx, double dy) {
        latitude -= dx;
        longitude -= dy;
    } 

    //SOMA OS VALORES DE UM PARAMETRO E DEVOLVE UM NOVO PONTO
    public Localizacao somaPonto (double dx, double dy) {
        return new Localizacao (latitude + dx, longitude + dy);
    }


    //VERIFICA SE DOIS PONTOS SÃO IGUAIS
    public boolean equals (Object o) {
       if (this == o) return true;
       
       if ((o == null) || (this.getClass() != o.getClass())) return false;
       
       Localizacao aux = (Localizacao) o;
       return (this.latitude == aux.getX() && this.longitude == aux.getY());
   }

    //CONVERTE PARA REPRESENTAÇÃO TEXTUAL
    public String toString () {
        return new String ( + latitude + ", " + longitude);
    }

    //CRIA UMA COPIA DO PONTO RECEPTOR (RECEPTOR = this)
    public Localizacao clone () {
        return new Localizacao (this);
    }
    
    
    
}