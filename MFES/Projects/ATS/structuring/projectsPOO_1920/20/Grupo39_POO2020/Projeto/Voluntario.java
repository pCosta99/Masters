
/**
 * Write a description of class Voluntário here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.io.Serializable;
public class Voluntario extends Transportador implements Serializable
{
    public Voluntario(String cod, String nome, GPS gps,double raio)
    {
        super(cod,nome, gps, raio);
    }

    public double getPreco(double dist, Encomenda e){
        return 0;
    }
}
