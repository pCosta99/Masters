import java.util.stream.Collectors;
import java.util.*;
import java.io.*;
import java.time.LocalDate;

public class Aceite extends Encomenda
{  
    /** Construtor vazio de Aceite*/
    public Aceite(){
        super();
    }
    
     /** Construtor com todas as variaveis de instancia de Aceite*/
    public Aceite(String ce, String cu, String cl, String ct, double peso, LocalDate data, List<LinhaEncomenda> le, int nProd, String estado){
        super(ce, cu, cl, ct, peso, data, le, nProd,estado);
    }
    
     /** Construtor com uma Aceite*/
    public Aceite(Aceite a){
        super(a);
    }
    
    /** 
     * Metodo retorna uma copia da Aceite
     * 
     * @return Uma copia da Aceite
     */
    public Aceite clone(){
        return new Aceite(this);
    }
    
    public String toString(){
        String s = "Código da Encomenda: " + getCodEnc();
        
        return s;
    }
    
    public boolean equals(Object o){
        
        boolean b = false;
        if(this == o){
            return true;
        }
        
        if(o == null || this.getClass() != o.getClass()){
            return false;
        }
        
        Aceite a = (Aceite) o;
        
        if(super.equals(a)) 
        return true;
        
        return b;
    }
}
