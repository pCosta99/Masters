import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.time.LocalDateTime;
import java.util.Set;
import java.util.TreeSet;
import java.util.Iterator;

public class EmpresaTransportadora extends Transportador{
    private int nif;
    private float precoPorKm;
    private int capacidade;
    private List<String> encAceites;//aceitou transportar encomenda com codigo
    private Map<String,Double> custoDeEncomendas;
    
    public EmpresaTransportadora(){
        super();
        this.nif=0;
        this.precoPorKm=0;
        this.capacidade=0;
        this.encAceites=new ArrayList<>();
        this.custoDeEncomendas =new HashMap<>();
    }
    
    public EmpresaTransportadora(GPS g, String nemail, String npassword,String nnome, String cod, float r, boolean tm
                    , GestaoEncomendas eR, GestaoEncomendas eE,int nnif,float npreco,int cap,GestaoEncomendas p,List<String> l,
                    Map<String,Double> m,float k,Map<String,Double> custo){
        super(g,nemail,npassword,nnome,cod,r,tm,eR,eE,m,k);
        this.nif=nnif;
        this.precoPorKm=npreco;
        this.capacidade=cap;        
        this.setEncAceites(l);
        this.setCustoDeEncomendas(custo);
    } 
    
    public EmpresaTransportadora(EmpresaTransportadora et){
        super(et);
        this.nif=et.getNif();
        this.precoPorKm=et.getPrecoPorKm();
        this.capacidade=et.getCapacidade();
        this.encAceites=et.getEncAceites();
        this.custoDeEncomendas=et.getCustoDeEncomendas();
    }
    
    public int getNif(){
        return this.nif;
    }
    
    public void setNif(int nnif){
        this.nif=nnif;
    }
    
    public float getPrecoPorKm(){
        return this.precoPorKm;
    }
    
    public void setPrecoPorKm(float ppk){
        this.precoPorKm=ppk;
    }
    
    public int getCapacidade(){
        return this.capacidade;
    }
    
    public void setCapacidade(int cap){
        this.capacidade=cap;
    }
    
    public List<String> getEncAceites (){
        List<String> l=new ArrayList<>();
        this.encAceites.stream().forEach(v->l.add(v));
        return l;
    }
    
    public void setEncAceites (List<String> enc){
        this.encAceites=new ArrayList<>();
        enc.stream().forEach(v->this.encAceites.add(v));

    }
    
    public Map<String,Double> getCustoDeEncomendas(){
        return this.custoDeEncomendas.entrySet().stream().collect(Collectors.toMap(e->e.getKey(),e->e.getValue()));
    }
    
    public void setCustoDeEncomendas(Map<String,Double> enc){
        this.custoDeEncomendas=new HashMap<>();
        enc.entrySet().forEach(e -> {this.custoDeEncomendas.put(e.getKey(),e.getValue());});
    }    
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        EmpresaTransportadora t = (EmpresaTransportadora) obj;
        return super.equals(t) &&
                    t.getNif()==this.nif &&
                    t.getPrecoPorKm()==this.precoPorKm &&
                    t.getCapacidade()==this.capacidade;
    }
    
    public EmpresaTransportadora clone(){
        return new EmpresaTransportadora(this);
    }
    
    public String toString (){
        StringBuilder sb=new StringBuilder();
        sb.append("Empresa Transportadora:").append("GPS:").append(this.getGps()).
                append(",").append("Email:").append(this.getEmail()).
                append(",").append("Nome:").append(this.getNome()).
                append(",").append("Password:").append(this.getPassword()).
                append(",").append("Codigo:").append(this.getCodigo()).
                append(",").append("Raio:").append(this.getRaio()).
                append(",").append("Transporta Medicamentos:").append(this.getTransportaMedicamentos()).
                append(",").append("Email:").append(this.getEmail()).
                append(",").append("Encomendas:").append(this.getGEncomendas()).
                append(",").append("Encomendas efetuadas:").append(this.getGEncomendas()).
                append(",").append("Nif:").append(this.nif).
                append(",").append("Preco por km:").append(this.precoPorKm).
                append(",").append("Capacidade:").append(this.capacidade).append("\n");
        return sb.toString();
    }
    
    public float custo (Encomenda enc){
        return 0;
    }
    
    public String paraEstado(){
        StringBuilder sb=new StringBuilder();
        sb.append("Transportadora:").append(super.paraEstado()).append(",").append(this.nif).append(",").append(this.getRaio()).
                                                            append(",").append(this.precoPorKm);
        return sb.toString();
    }
    
    public String estadoAtual(){
        StringBuilder sb=new StringBuilder();
        sb.append("Capacidade:").append(this.capacidade).append("\n");
        return sb.toString();
    }
     
    
    public void aceitarEncomenda(String cod){
        this.encAceites.add(cod);
    }
    
    public void transporteAceite(Encomenda cod){
        super.transporteAceite(cod);
        this.encAceites.remove(cod.getCodEncomenda());
    }
    
    public void addCustoEncomenda(String cod, float f){
        this.custoDeEncomendas.put(cod,Double.valueOf(f));
    }
    
    public void estimarPreco(){
        
    }
    
    public String getPreco(){
        StringBuilder sb=new StringBuilder();
        String a;
        if (this.precoPorKm<0) a="n/a";
        else a=Float.toString(this.precoPorKm);        
        sb.append("Codigo do transportador: ").append(this.getCodigo()).append("\n      Preco por kilometro: ").append(a)
        .append("\n");
        return sb.toString();
    }
    
    
    public String getfatura (LocalDateTime t1,LocalDateTime t2){
        float sum =0;
        EncomendaEfetuada e;
        StringBuilder sb=new StringBuilder();
        TreeSet<EncomendaEfetuada> enc=new TreeSet <EncomendaEfetuada>(new DateTimeComparatorEE());
        this.getEncomendasEfetuadas().getEncomendas().values().stream().map(v->(EncomendaEfetuada) v)
                    .filter(v->(v.getTempo().isAfter(t1) && v.getTempo().isBefore(t2))).forEach(v->enc.add(v));
        Iterator<EncomendaEfetuada> itr=enc.iterator();
        sb.append("Encomendas efetudas:\n");
        while (itr.hasNext()){
            e=itr.next();
            sb.append(e.getTempo().toString()).append(": ").append(e.toString()).append("\n");
            if (this.custoDeEncomendas.containsKey(e.getCodEncomenda())) sum+=this.custoDeEncomendas.get(e.getCodEncomenda());
        }
        sb.append("TOTAL: ").append(sum).append("\n");
        return sb.toString();
    }
    
    public void addEncomendaEfetuada(EncomendaEfetuada enc){
        super.addEncomendaEfetuada(enc);
        this.custoDeEncomendas.put(enc.getCodEncomenda(),new Double(enc.getCusto()));
    }
    
    public boolean excedeCapacidade(){
        return this.encAceites.size()+super.getTotalEncomendas()+1>this.capacidade;
    }
    
    public void removeEncAceite(String cod){
        this.encAceites.remove(cod);
    }
}






















