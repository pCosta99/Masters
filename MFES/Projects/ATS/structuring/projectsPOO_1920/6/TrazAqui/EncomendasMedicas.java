
/**
 * Classe das Encomendas Medicas
 * 
 * @author (João Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
public class EncomendasMedicas extends Encomenda implements Serializable
{
    //construtor por omissão
    public EncomendasMedicas(){
        super();
    }
    
    //construtor parametrizado
    public EncomendasMedicas(Encomenda e){
        super(e);
    }
    
    //construtor de copia
    public EncomendasMedicas(EncomendasMedicas em){
        super(em);
    }
}