import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

//classe que guarda os dados de todos os voluntarios


public class DadosVoluntarios implements Serializable {

    private Map<String, Voluntario> voluntarios;

    
//construtor vazio
    
    public DadosVoluntarios(){
        this.voluntarios = new HashMap<String, Voluntario>();
    }
    
 //construtor parametrizado

    public DadosVoluntarios(DadosVoluntarios d){
        this.voluntarios = d.getVoluntarios();
    }

 //construtor de cópia
    
    public DadosVoluntarios(Map<String, Voluntario> v){
        setVoluntarios(v);
    }

//metodo que da set no hashMap ao voluntário em causa
    
    public void setVoluntarios(Map<String, Voluntario> m){
        this.voluntarios = new HashMap<String, Voluntario>();
        m.values().forEach(d -> this.voluntarios.put(d.getId(), d.clone()));

    }
    
 //metodo que devolve um hashMap com todos os voluntários no sistema

    public Map<String, Voluntario> getVoluntarios(){
        return this.voluntarios;
    }

 //Método que verifica se 2 objetos sao iguais

    public boolean equals(Object object){
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;

        DadosVoluntarios d = (DadosVoluntarios) object;
        Map<String, Voluntario> d1 = d.getVoluntarios();

        return d1.equals(this.voluntarios);
    }


//metodo que devolve a classe em si numa string    

    public String toString(){
        StringBuilder sb = new StringBuilder();

        for(Voluntario u : this.voluntarios.values())
            sb.append(u.toString()).append("\n");

        return sb.toString();
    }



    //metodo que devolve o voluntario loggado no momento
    
    public Voluntario getVoluntario(String id) throws IdNotFoundException{
        Voluntario v = this.voluntarios.get(id);
        if(v == null){
            throw new IdNotFoundException(id);
        }
        return v.clone();
    }



    //metodo que adiciona um voluntario ao sistema

    public void addVoluntario(Voluntario v) throws InvalidValueException{
        String i = v.getId();
        if(this.voluntarios.containsKey((i))){
            throw new InvalidValueException(i);
        }
        this.voluntarios.put(i,v.clone());
    }

    public DadosVoluntarios clone(){
        return new DadosVoluntarios(this);
    }


}
