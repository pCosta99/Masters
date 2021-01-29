
/**
 * Write a description of class Pair here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.io.Serializable;


public class Pair implements Serializable
{
    private String x;
    private String y;

    /**
     * Constructor for objects of class Pair
     */
    public Pair(String x, String y)
    {
        this.x = x;
        this.y = y;
    }

    public String p1(){
      return this.x;
    }
    
    public String p2()
    {
         return this.y;
    }
    
    public boolean equals(Object p){
        if(this == p){
            return true;
        }
        if (p == null){
            return false;
        }
        if(p.getClass() != this.getClass()){
            return false;
        }
        
        Pair that = (Pair) p;        
        return this.x.equals(that.p1()) && this.y.equals(that.p2());
        
    }
}
