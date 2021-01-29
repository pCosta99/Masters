
/**
 * Classe das Encomendas Aceites
 * 
 * @author (João Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
public class EncomendasAceites extends Encomenda implements Serializable
{
    // variáveis de instância das Encomendas Aceites
    private String codEncomenda;
    
    // construtor por omissão
    public EncomendasAceites(){
        this.codEncomenda = "n/a";
    }
    
    // construtor parametrizado
    public EncomendasAceites(String codEnc){
        this.codEncomenda = codEnc;
    }
    
    // construtor de copia
    public EncomendasAceites(EncomendasAceites ea){
        this.codEncomenda = ea.getCodEnc();
    }
    
    // metodo que devolve o codigo da encomenda
    public String getCodEnc(){
        return this.codEncomenda;
    }
    
    // metodo para definir o codigo de encomenda
    public void setCodEnc(String codEnc){
        this.codEncomenda = codEnc;
    }
    
    // metodo que devolve uma string com a informacao de uma encomenda aceite
    public String toString(){
        return ("Aceite:"+this.codEncomenda).toString();
    }
    
    // metodo de copia de encomendas aceites
    public EncomendasAceites clone(){
        return new EncomendasAceites(this);
    }
    
    // metodo que compara se duas encomendas que foram aceites sao iguais
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o.getClass()!=this.getClass() || o==null)) return false;
        EncomendasAceites ea = (EncomendasAceites) o;
        return this.codEncomenda.equals(ea.getCodEnc());
    }
}
