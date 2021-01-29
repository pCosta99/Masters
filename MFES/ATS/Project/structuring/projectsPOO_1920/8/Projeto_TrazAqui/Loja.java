import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Loja extends User implements Serializable {

    private List<Encomenda> filaespera;
    private double tempoAtendimento;

    private Loja(){}

    public Loja(String username, String nome, Coordenadas pos){
        super(username,nome,pos);
        this.filaespera = new ArrayList<>();
        this.tempoAtendimento = 10;
    }


    public Loja(String nome, String username, String password, Coordenadas pos, List<Encomenda> fl, double tempoA){
        super(nome,username,password,pos);
        this.tempoAtendimento = tempoA;
        setFilaespera(fl);
    }

    public Loja(Loja l1){
        super(l1);
        this.tempoAtendimento = l1.getTempoAtendimento();
        setFilaespera(l1.getFilaespera());
    }

    public double getTempoAtendimento() {
        return tempoAtendimento;
    }

    public void setTempoAtendimento(double tempoAtendimento) {
        this.tempoAtendimento = tempoAtendimento;
    }

    public List<Encomenda> getFilaespera(){
        List<Encomenda> ret = new ArrayList<>();
        for(Encomenda e : this.filaespera){
            ret.add(e.clone());
        }
        return ret;
    }

    public void setFilaespera(List<Encomenda> l){
        this.filaespera = l.stream().map(Encomenda :: clone).collect(Collectors.toList());
    }

    /**
     * Metodo para inserir encomenda na lista de espera
     */
    public void addEncomenda(Encomenda e){
        if (!this.filaespera.contains(e)){
            this.filaespera.add(e.clone());
        }else{
            System.out.println("Essa encomenda já está na fila de espera\n");
        }
    }

    /**
     * Metodo para remover encomenda da lista de espera
     */
    public void removeEncomenda(Encomenda enc){
        boolean contem = false;
        if(this.filaespera.contains(enc)){
                this.filaespera.remove(enc);
        }
        else{
            System.out.println("A encomenda que quer remover não está na fila de espera\n");
        }
    }

    public Loja clone(){
        return new Loja(this);
    }

    public boolean equals(Object o){
        if (o==this)return true;
        if(o==null || o.getClass()!=this.getClass())return false;
        Loja l = (Loja) o;
        return l.getUsername().equals(((Loja) o).getUsername());
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Loja ---> ").append(super.toString()).append("\n");
        return sb.toString();
    }

    public int hashCode(){
        int hash = 5;
        long aux1;
        hash = 31 * hash + super.hashCode();
        aux1 = Double.doubleToLongBits(this.tempoAtendimento);
        hash = 31*hash + (int)(aux1 ^ (aux1 >>> 32));
        hash = 31*hash + this.filaespera.stream().mapToInt(Encomenda::hashCode).sum();
        return hash;
    }
}
