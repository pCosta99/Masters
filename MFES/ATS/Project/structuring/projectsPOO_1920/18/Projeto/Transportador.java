import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.util.NoSuchElementException;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.time.LocalDateTime;


public abstract class Transportador extends Usuario{

    private float raio;
    private boolean transportaMedicamentos;
    private Map<String,Double> classificacao;
    private float kmsPercorridos;
    
    public Transportador(){
        super();
        this.raio=0;
        this.transportaMedicamentos=false;
        this.classificacao=new HashMap<>();
        this.kmsPercorridos=0;
    }
    
    public Transportador(GPS g, String nemail, String npassword,String nnome, String cod, float r, boolean tm
                    , GestaoEncomendas eR, GestaoEncomendas eE,Map<String,Double> m,float k){
        super(nnome,cod,g,nemail,npassword,eR,eE);
        this.raio=r;
        this.transportaMedicamentos=tm;
        this.setClassificacao(m);
        this.kmsPercorridos=k;
    } 

    public Transportador (Transportador t){
        this.setNome(t.getNome());
        this.setCodigo(t.getCodigo());
        this.raio=(t.getRaio());
        this.transportaMedicamentos=t.getTransportaMedicamentos();
        this.setGps(t.getGps());
        this.setPassword(t.getPassword());
        this.setEmail(t.getEmail());
        this.setGEncomendas(t.getGEncomendas());
        this.setEncomendasEfetuadas(t.getEncomendasEfetuadas());
        this.classificacao=t.getClassificacao();
        this.kmsPercorridos=t.getKmsPercorridos();
    }
    
    public float getRaio(){
        return this.raio;
    }
    
    public void setRaio(float r){
        this.raio=r;
    }
    
    public boolean getTransportaMedicamentos(){
        return this.transportaMedicamentos;
    }
    
    public void setTransportaMendicamentos(boolean tm){
        this.transportaMedicamentos=tm;
    }
    
    public Map<String,Double> getClassificacao(){
        return this.classificacao.entrySet().stream().collect(Collectors.toMap(e->e.getKey(),e->e.getValue()));
    }
    
    public void setClassificacao(Map<String,Double> enc){
        this.classificacao=new HashMap<>();
        enc.entrySet().forEach(e -> {this.classificacao.put(e.getKey(),e.getValue());});
    }
    
    public float getKmsPercorridos(){
        return this.kmsPercorridos;
    }
    
    public void setKmsPercorridos (float k){
        this.kmsPercorridos=k;
    }
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Transportador t = (Transportador) obj;
        return super.equals(t)  && t.getCodigo().equals(this.getCodigo()) &&
                    t.getNome().equals(this.getNome()) &&
                    t.getRaio()==(this.raio) &&
                    t.getTransportaMedicamentos()==this.transportaMedicamentos &&
                    t.getClassificacao().equals(this.classificacao) &&
                    t.getKmsPercorridos()==this.kmsPercorridos;
    }
    
    public abstract Transportador clone();
    
    public boolean recolherEncomenda(String cod){
        return true;
    }
    
    public boolean recolherEncomenda(Encomenda enc){
        return true;
    } 
    
    public boolean aceitoTransporteDeMedicamentos(){
        return this.transportaMedicamentos;
    }
    
    public void transporteAceite(Encomenda enc){
        this.addEncomenda(enc);
    }
        
    public Double getValorClassificacao() throws NoSuchElementException{
        try {return this.classificacao.values().stream().mapToDouble(a->a).average().getAsDouble();}
        catch (NoSuchElementException e){return 0.0;}
    }
    
    public void addClassificacao(String cod,Double v){
        this.classificacao.put(cod,v);
    }
    
    public String getfatura (LocalDateTime t1,LocalDateTime t2){
        StringBuilder sb=new StringBuilder();
        TreeSet<EncomendaEfetuada> enc=new TreeSet <EncomendaEfetuada>(new DateTimeComparatorEE());
        this.getEncomendasEfetuadas().getEncomendas().values().stream().map(v->(EncomendaEfetuada) v)
                    .filter(v->(v.getTempo().isAfter(t1) && v.getTempo().isBefore(t1))).forEach(v->enc.add(v));
        Iterator<EncomendaEfetuada> itr=enc.iterator();
        EncomendaEfetuada e;
        sb.append("Fatura:\n");
        while (itr.hasNext()){
            e=itr.next();
            sb.append(e.getTempo().toString()).append(": ").append(e.toString()).append("\n");
        }
        return sb.toString();
    }
    
    public void addKmsPercorridos(float d){
        this.kmsPercorridos+=d;
    }
}





















































