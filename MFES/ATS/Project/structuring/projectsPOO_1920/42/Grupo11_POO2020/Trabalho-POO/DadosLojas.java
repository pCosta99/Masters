import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class DadosLojas implements Serializable {

    private Map<String, Loja> lojas;

//construtor vazio
    
    public DadosLojas(){
        this.lojas = new HashMap<String, Loja>();
    }
    
//construtor parametrizado

    public DadosLojas(DadosLojas d){
        this.lojas = d.getLojas();
    }

//construtor de cõpia
    
    public DadosLojas(Map<String, Loja> e){
        setLojas(e);
    }

//metodo que dá set a uma loja em causa dentro da hashmap    

    public void setLojas(Map<String, Loja> m){
        this.lojas = new HashMap<String, Loja>();
        m.values().forEach(d -> this.lojas.put(d.getId(), d.clone()));

    }
    
//metodo que devolve o hashmap das lojas todas

    public Map<String, Loja> getLojas(){
        return this.lojas;
    }

//metodo que verifica se 2 objetos sao iguais

    public boolean equals(Object object){
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;

        DadosLojas d = (DadosLojas) object;
        Map<String, Loja> d1 = d.getLojas();

        return d1.equals(this.lojas);
    }

//metodo que devolve a classe numa string

    public String toString(){
        StringBuilder sb = new StringBuilder();

        for(Loja u : this.lojas.values())
            sb.append(u.toString()).append("\n");

        return sb.toString();
    }



    //metodo que devolve a loja loggada no momento
    
    public Loja getLoja(String id) throws IdNotFoundException{
        Loja e = this.lojas.get(id);
        if(e == null){
            throw new IdNotFoundException(id);
        }
        return e;
    }



    //metodo que adiciona uma loja ao sistema

    public void addLoja(Loja e) throws InvalidValueException{
        String i = e.getId();
        if(this.lojas.containsKey((i))){
            throw new InvalidValueException(i);
        }
        this.lojas.put(i,e.clone());
    }


    public DadosLojas clone(){
        return new DadosLojas(this);
    }

}
