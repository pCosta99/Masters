import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class DadosEncomendas implements Serializable{

    private Map<String, Encomenda> encomendas;



    public DadosEncomendas(){
            this.encomendas = new HashMap<String, Encomenda>();
        }

//construtor parametrizado

    public DadosEncomendas(DadosEncomendas d){
            this.encomendas = d.getEncomendas();
        }

//construtor de cõpia

        public DadosEncomendas(Map<String, Encomenda> e){
            setEncomendas(e);
        }

//metodo que dá set a uma loja em causa dentro da hashmap

        public void setEncomendas(Map<String, Encomenda> m){
            this.encomendas = new HashMap<String, Encomenda>();
            m.values().forEach(d -> this.encomendas.put(d.getId(), d.clone()));

        }

//metodo que devolve o hashmap das lojas todas

        public Map<String, Encomenda> getEncomendas(){
            return this.encomendas;
        }

//metodo que verifica se 2 objetos sao iguais

        public boolean equals(Object object){
            if (this == object) return true;
            if (object == null || getClass() != object.getClass()) return false;

            DadosEncomendas d = (DadosEncomendas) object;
            Map<String, Encomenda> d1 = d.getEncomendas();

            return d1.equals(this.encomendas);
        }

//metodo que devolve a classe numa string

        public String toString(){
            StringBuilder sb = new StringBuilder();

            for(Encomenda u : this.encomendas.values())
                sb.append(u.toString()).append("\n");

            return sb.toString();
        }





        public Encomenda getEncomenda(String id) throws IdNotFoundException{
            Encomenda e = this.encomendas.get(id);
            if(e == null){
                throw new IdNotFoundException("Encomenda nao existe");
            }
            return e;
        }





        public void addEncomenda(Encomenda e) throws InvalidValueException{
            String i = e.getId();
            if(this.encomendas.containsKey((i))){
                throw new InvalidValueException(i);
            }
            this.encomendas.put(i,e);
        }


        public DadosEncomendas clone(){
            return new DadosEncomendas(this);
        }

    }

