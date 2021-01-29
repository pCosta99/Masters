import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Utilizadores extends AllUsers implements Serializable
{   
    private List <String> porclassificar;
    private List<Encomenda> historico;

    public Utilizadores()
    {
        super();
        this.porclassificar=new ArrayList<>();
        this.historico = new ArrayList<>();
    }
    
    public Utilizadores (String nome, String codigo, Coordenadas gps, String mail, String pw, List <String> por, List<Encomenda> history, List <Integer> aval)
    {
        super(nome, codigo, gps, mail, pw, aval);
        setPorClassificar(por);
        setHistorico(history);
    }
    
    public Utilizadores (Utilizadores u)
    {
        super(u);
        setPorClassificar(u.getPorClassificar());
        setHistorico(u.getHistorico());
    }

    public List <String> getPorClassificar(){
        List <String> por =new ArrayList<> ();
        for (String e : this.porclassificar){
            por.add(e);
        }
        return por;
    }

    public List<Encomenda> getHistorico(){
        List<Encomenda> newhis = new ArrayList<>();
        for (Encomenda e : this.historico){
            newhis.add(e);
        }
        return newhis;
    }

    public void setPorClassificar(List <String> por){
        this.porclassificar=new ArrayList<> ();
        for (String e : por){
            this.porclassificar.add(e);
        }
    }

    public void setHistorico(List<Encomenda> hist){
        this.historico = new ArrayList<>();
        for (Encomenda e : hist){
            this.historico.add(e);
        }
    }

    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        
        Utilizadores u = (Utilizadores) o;
        return(super.equals(u));
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("\n");
        return sb.toString();
    }
    
    public Utilizadores clone()
    {
        return new Utilizadores (this);
    }
    
    public String tipoUtilizador()
    {
        return "Utilizador";
    }

    @Override
    public ArrayList<Integer> getClassificacao(){
        return new ArrayList<>();
    }

    public void adicionaPorClassificar(String e){
        List <String> por = getPorClassificar();
        por.add(e);
        setPorClassificar(por);
    }

    public void removePorClassificar (String e){
        List <String> por = getPorClassificar();
        por.remove(e);
        setPorClassificar(por);
    }
    
    public void addHistorico(Encomenda e){
        this.historico.add(e);
    }
        
    
    
}
