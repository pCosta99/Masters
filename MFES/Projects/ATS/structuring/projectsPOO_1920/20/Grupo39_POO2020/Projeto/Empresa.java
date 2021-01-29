
/**
 * Write a description of class Empresa here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.io.Serializable;
public class Empresa extends Transportador implements Serializable
{    
    private double totalGanho = 0;
    private String nif;
    private double preco;
    
    /**
     * Constructor for objects of class Empresa
     */
    public Empresa(String cod, String nome, GPS gps, double raio, String nif, double preco)
    {
        super (cod, nome, gps, raio);
        this.nif = nif;
        this.preco = preco;
    }
    
    public double getPreco(double dist, Encomenda e){
        double res = -1;
        try{
            res = e.getPeso()*0.10 + dist*this.preco;
        }
        catch (ExceptionNaoDefinido exc){
            
        }
        return res;
    }
    
    public void addLucro(String lucro){
        this.totalGanho = this.totalGanho + Double.parseDouble(lucro);
    }
    
    public double getLucro(){
        return this.totalGanho;
    }
}
