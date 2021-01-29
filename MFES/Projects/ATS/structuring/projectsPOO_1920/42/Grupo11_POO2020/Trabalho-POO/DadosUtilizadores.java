
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;


//classe que guarda os dados de todos os utilizadores

public class DadosUtilizadores implements Serializable{

    private Map<String, Utilizador> utilizadores;

    public DadosUtilizadores(){
        this.utilizadores = new HashMap<String, Utilizador>();
    }

    public DadosUtilizadores(DadosUtilizadores d){
        this.utilizadores = d.getUtilizadores();
    }

    public DadosUtilizadores(Map<String, Utilizador> u){
        setUtilizadores(u);
    }


    public void setUtilizadores(Map<String, Utilizador> m){
        this.utilizadores = new HashMap<String, Utilizador>();
        m.values().forEach(d -> this.utilizadores.put(d.getId(), d.clone()));

    }

    public Map<String, Utilizador> getUtilizadores(){
        return this.utilizadores;
    }


    public boolean equals(Object object){
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;

        DadosUtilizadores d = (DadosUtilizadores) object;
        Map<String, Utilizador> d1 = d.getUtilizadores();

        return d1.equals(this.utilizadores);
    }


    public String toString(){
        StringBuilder sb = new StringBuilder();

        for(Utilizador u : this.utilizadores.values())
            sb.append(u.toString()).append("\n");

        return sb.toString();
    }



    //getUtilizador
    public Utilizador getUtilizador(String id) throws IdNotFoundException{
        Utilizador u = this.utilizadores.get(id);
        if(u == null){
            throw new IdNotFoundException(id);
        }
        return u;
    }



    //addUtilizador

    public void addUtilizador(Utilizador u) throws InvalidValueException{
        String i = u.getId();
        if(this.utilizadores.containsKey((i))){
            throw new InvalidValueException(i);
        }
        this.utilizadores.put(i,u.clone());
    }


    public DadosUtilizadores clone(){
        return new DadosUtilizadores(this);
    }
}
