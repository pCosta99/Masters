import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Aceite implements Serializable {
    private ArrayList<String> aceites;

    public ArrayList<String> getAceites () {
        ArrayList<String> a = new ArrayList<>();
        for (String b : this.aceites) {
           a.add(b);
        }
        return a;
    }

    public Aceite () {
        this.aceites = new ArrayList<>();
    }

    /**Método que adiciona um código válido a lista de aceites
    *@param a Código Encomenda */
    public void adicionaAceite (String a){
        this.aceites.add(a);
    }

    /**Método que retorna uma lista das encomendas default*/
    public List<String> encDef (){
        return this.aceites.stream().collect(Collectors.toList());
    }
}

