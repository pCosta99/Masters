import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;


public class CatEncomenda implements Serializable {
    /**Map que representa o catálogo de encomendas, sendo as keys os códigos e os values as respectivas encomendas */
    private Map<String,Encomenda> encs;

    public CatEncomenda () {
        this.encs = new HashMap<>();
    }

    /**Método que adiciona uma nova encomenda ao Map de encomendas
     *@param a Encomenda */
    public void adicionaEnc (Encomenda a){
        this.encs.put(a.getCod(),a.clone());
    }

    /**Método que gera um código único para uma encomenda*/
    public String codUnicoE (){
        String a = null;
        while (this.encs.containsKey(a) || a == null) {
            StringBuilder sb = new StringBuilder();
            sb.append("e").append(ThreadLocalRandom.current().nextInt(1000, 9999));
            a = sb.toString();
        }
        return a;
    }

    /**Método que dado uma lista de códigos de encomenda retorna uma lista dos registos respectivos usando a informação do catálogo
     *@param a lista de códigos de encomenda */
    public List<Registos> adicionaReg(List<String> a){
        List<Registos> reg = new ArrayList<>();
        this.encs.values().stream().filter(x->(a.contains(x.getCod()))).forEach(x->reg.add(new Registos(x.clone(), LocalDateTime.now(),x.getUser(),"n/a",0.0,0.0,0.0)));
        return reg;
    }
}
