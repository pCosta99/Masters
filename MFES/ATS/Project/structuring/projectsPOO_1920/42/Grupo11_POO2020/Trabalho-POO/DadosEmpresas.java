import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;


//classe que guarda os dados de todas as empresas

public class DadosEmpresas implements Serializable {
    private Map<String, Empresa> empresas;

//construtor vazio
    
    public DadosEmpresas(){
        this.empresas = new HashMap<String, Empresa>();
    }

//construtor parametrizado
    
    public DadosEmpresas(DadosEmpresas d){
        this.empresas = d.getEmpresas();
    }
    
 //construtor de cópia

    public DadosEmpresas(Map<String, Empresa> e){
        setEmpresas(e);
    }


//metodo que da set no hashmap das empresa a uma empresa em causa
    
    public void setEmpresas(Map<String, Empresa> m){
        this.empresas = new HashMap<String, Empresa>();
        m.values().forEach(d -> this.empresas.put(d.getId(), d.clone()));

    }
    
//metodo que devolve o hashmap das empresas

    public Map<String, Empresa> getEmpresas(){
        return this.empresas;
    }
    
//metodo que verifica se 2 objetos sao iguais
    
    public boolean equals(Object object){
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;

        DadosEmpresas d = (DadosEmpresas) object;
        Map<String, Empresa> d1 = d.getEmpresas();

        return d1.equals(this.empresas);
    }
    
//metodo que devolve a classe numa String

    public String toString(){
        StringBuilder sb = new StringBuilder();

        for(Empresa u : this.empresas.values())
            sb.append(u.toString()).append("\n");

        return sb.toString();
    }



    //metodo que devolve a empresa loggada no momento
    
    public Empresa getEmpresa(String id) throws IdNotFoundException{
        Empresa e = this.empresas.get(id);
        if(e == null){
            throw new IdNotFoundException(id);
        }
        return e;
    }



    //metodo que adiciona uma empresa ao hashmap das empresas

    public void addEmpresa(Empresa e) throws InvalidValueException{
        String i = e.getId();
        if(this.empresas.containsKey((i))){
            throw new InvalidValueException(i);
        }
        this.empresas.put(i,e.clone());
    }


    public DadosEmpresas clone(){
        return new DadosEmpresas(this);
    }
}
