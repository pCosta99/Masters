import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;


/**
 * Classe usada para facilitar o uso duma lista de Encomendas, 
 * com métodos auxiliares, esta classe é usada no model (TrazAqui.java)
 * na variável aindaNaoEntregues sendo um objeto desta classe
 */

public class EncomendasAceites implements Serializable {

    
    List <Encomenda> encomendas;  

    public EncomendasAceites() {
        this.encomendas = new ArrayList<>();
    }

    public EncomendasAceites(EncomendasAceites e) {
        this.encomendas = getEncomendas();
    }

    public EncomendasAceites(List<Encomenda> encomendas) {
        setEncomendas(encomendas);
    }



    //SETTERS AND GETTERS
    public List<Encomenda> getEncomendas() {
        List<Encomenda> ret = new ArrayList<>();
  
        for (Encomenda e : this.encomendas){
            ret.add(e.clone());
        }

        return ret;
    }

    public void setEncomendas (List<Encomenda> encomendas) {
        this.encomendas = encomendas.stream().map( e -> e.clone()).collect(Collectors.toList());
    }

    public int getSize(){
        return this.encomendas.size();
    }

    //CLONE
    public EncomendasAceites clone (){
        return new EncomendasAceites (this);
    }

    //TOSTRING
    public String toString() {
   
        StringBuilder sb = new StringBuilder();
        Iterator<Encomenda> it = this.encomendas.iterator();
        
        while (it.hasNext()){
            Encomenda now = it.next();
            sb.append("\nEncomenda Aceite: ").append(now.toString() + "  ");
        }


        return sb.toString();
    }  


    //EQUALS
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof EncomendasAceites)) {
            return false;
        }
        EncomendasAceites e = (EncomendasAceites) o;
        return e.getEncomendas().equals(this.encomendas);
    }

    //FUNCOES
    /**
     *   Este método é usado para colocar as encomendas já aceites em um ficheiro CSV,
     *   ou seja encomendas que o utilizador já solicitou a sua entrega 
     *   ficando como Aceite:"codigo da Encomenda" 
     */

    public String paraCSV (){

        StringBuilder sb = new StringBuilder();

        for (Encomenda e : this.encomendas){
            sb.append("Aceite:").append(e.getCodEncomenda()).append("\n");
        }

        return sb.toString();
    }

    
    public void addEncomendaAceite (Encomenda encomendaAceite){
        this.encomendas.add(encomendaAceite.clone());
    }


    /**
     * Procuramos certa encomenda a partir do seu código e damos 
     * return ao objecto correspondente se encontrado ou a null
     */
    
    public Encomenda buscaEncomenda (String codEnc){
        for (Encomenda e : this.encomendas){
            if (e.getCodEncomenda().equals(codEnc))
                return e.clone();
        } 
        return null;   
    }

    public void removeEncomenda (Encomenda e){
        this.encomendas.remove(e);
    }
}