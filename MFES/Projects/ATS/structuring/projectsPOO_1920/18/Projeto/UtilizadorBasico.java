import java.lang.String;
import java.util.Map;
import java.util.HashMap;   
import java.util.stream.Collectors;
import java.util.List;
import java.util.ArrayList;
import java.util.Comparator;


public class UtilizadorBasico extends Usuario {  
    
    List<String> encomendasProntas;//Encomendas em que o transporte ainda nao foi aceite mas ja estao prontas para recolher
    List<String> encomendasComTransportador;
    Map<String,List<Proposta>> propostasDeTransporte;//codEncomenda -> codTransportadores
    
    public UtilizadorBasico(){
        super();
        this.encomendasProntas = new ArrayList<>();
        this.propostasDeTransporte = new HashMap<>();
        this.encomendasComTransportador=new ArrayList<>();
    }
    
    public UtilizadorBasico(GPS g, String nemail, String npassword, GestaoEncomendas ge,GestaoEncomendas gee,
       String nnome,String cod,List<String> nE,Map<String,List<Proposta>> m,List<String> l){
        super(nnome,cod,g,nemail,npassword,ge,gee);
        this.setEncomendasProntas(nE);
        this.setPropostasDeTransporte(m);
        this.setEncomendasComTransportador(l);
    }    
    
    public UtilizadorBasico(UtilizadorBasico l){
        super(l.getNome(),l.getCodigo(),l.getGps(),l.getEmail(),l.getPassword(),l.getGEncomendas(),l.getEncomendasEfetuadas());
        this.encomendasProntas=l.getEncomendasProntas();
        this.propostasDeTransporte=l.getPropostasDeTransporte();
        this.encomendasComTransportador=l.getEncomendasComTransportador();
    }
    
    public List<String> getEncomendasProntas(){
        return new ArrayList<String> (this.encomendasProntas);
    }
    
    public void setEncomendasProntas(List<String> ep){
        this.encomendasProntas=new ArrayList<>(ep);
    }
    
    public Map<String,List<Proposta>> getPropostasDeTransporte(){
        return this.propostasDeTransporte.entrySet().stream().collect(Collectors.toMap(e->e.getKey(),e->e.getValue()));
    }
    
    public void setPropostasDeTransporte(Map<String,List<Proposta>> pro){
        this.propostasDeTransporte=new HashMap<>();
        pro.entrySet().forEach(e -> {this.propostasDeTransporte.put(e.getKey(),e.getValue());});
    }
    
    public List<String> getEncomendasComTransportador(){
        return new ArrayList<>(this.encomendasComTransportador);
    }
    
    public void setEncomendasComTransportador(List<String> enc){
        this.encomendasComTransportador=new ArrayList<>(enc);
    }
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        UtilizadorBasico lo = (UtilizadorBasico) obj;
        return super.equals(lo) && this.encomendasProntas.equals(lo.getEncomendasProntas());
    }
    
    public String toString(){
        StringBuilder sb=new StringBuilder();
        sb.append("Utilizador:").append("Codigo:").append(this.getCodigo()).append(",")
                .append("Nome:").append(this.getNome()).append(",")
                .append(this.getGps()).append(",")
                .append("Password:").append(this.getPassword()).append(",")
                .append("Encomendas:").append(this.getGEncomendas().toString()).append(",")
                .append("Encomendas Efetuadas:").append(this.getEncomendasEfetuadas().toString()).append(",")
                .append("GPS:").append(this.getGps()).append(",")
                .append("Propostas de Transporte:").append(this.getPropostasDeTransporte()).append(",")
                .append("Email:").append(this.getEmail()).append(",")
                .append("Encomendas com transportador: ").append(this.encomendasComTransportador).append(",")
                .append("Encomendas prontas: ").append(this.encomendasProntas).append("\n");
                return sb.toString();
    }
    
    public UtilizadorBasico clone (){
        return new UtilizadorBasico(this);
    }
    
    public String paraEstado(){
        StringBuilder sb=new StringBuilder();
        sb.append("Utilizador:").append(super.paraEstado());
        return sb.toString();
    }
    
    /*retorna o numero total de encomendas prontas*/
    public int getNEncomendasProntas(){
        return this.encomendasProntas.size();
    }
    
    public boolean encomendaPronta(String cod){
        return this.encomendasProntas.contains(cod);
    }
    
    public List<Proposta> aceitouEncomenda (String cod){
        this.encomendasProntas.remove(cod);
        this.encomendasComTransportador.add(cod);
        return this.propostasDeTransporte.remove(cod);
    }
    
    public String getStrPropostasDeTransporte(){
        StringBuilder sb = new StringBuilder();
        sb.append("Propostas de transporte:\n");
        this.propostasDeTransporte.entrySet().stream().forEach(e -> sb.append("Codigo de encomenda: ").append(e.getKey()).
                       append(": Propostas -> ").append(e.getValue().toString()).append("\n"));
        return sb.toString();
    }
    
    public boolean propostasContains(String cod){
        return this.propostasDeTransporte.containsKey(cod);        
    }
    
    public boolean propostasContainsTra(String codEnc,String codTra){
        return this.propostasDeTransporte.get(codEnc).stream().map(Proposta::getCod).anyMatch(e -> e.equals(codTra));  
    }
    
    public void novaPropostaDeTransporte(String codE,String codT,float custo){
        if (!(this.propostasDeTransporte.containsKey(codE))){
            List<Proposta> p= new ArrayList<>();
            this.propostasDeTransporte.put(codE,p);
        }
        this.propostasDeTransporte.get(codE).add(new Proposta(codT,custo));
    }
    
    public boolean containsEncomendaPronta(String cod){
        return this.encomendasProntas.contains(cod);
    }
    
    public void addEncomendaPronta(String codEnc){
        this.encomendasProntas.add(codEnc);
    }
    
    public void addEncComTransportador(String cod){
        this.encomendasComTransportador.add(cod);
    }
}




















