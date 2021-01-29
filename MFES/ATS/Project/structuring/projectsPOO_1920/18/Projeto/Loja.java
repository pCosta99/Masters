import java.lang.String;
import java.util.Map;
import java.util.HashMap;   
import java.util.stream.Collectors;
import java.util.List;
import java.util.ArrayList;

public class Loja extends Usuario{
    
    private List<String> encomendasSolicitadas;//encomendas que precissam de transportador
    private List<String> encomendasComTransportador;
    private Map<String,List<List<LinhaEncomenda>>> pedidos;//novos pedidos de encomednas por partes de utilizadores
    
    public Loja(){
        super();
        this.encomendasSolicitadas=new ArrayList<>();
        this.encomendasComTransportador=new ArrayList<>();
        this.pedidos=new HashMap<>();
    }
    
    public Loja(String cod, String nome,GPS gps, String email, String pass, GestaoEncomendas ge,GestaoEncomendas gee,
    List<String> l,List<String> l2,Map<String,List<List<LinhaEncomenda>>> p){
        super(nome,cod,gps,email,pass,ge,gee);
        this.setEncomendasSolicitadas(l);
        this.setEncomendasComTransportador(l2);
        this.setPedidos(p);
    }
    
    public Loja(Loja l){
        this.setCodigo(l.getCodigo());
        this.setNome(l.getNome());
        this.setGps(l.getGps());
        this.setPassword(l.getPassword());
        this.setEmail(l.getEmail());
        this.setGEncomendas(l.getGEncomendas());
        this.setEncomendasEfetuadas(l.getEncomendasEfetuadas());
        this.encomendasSolicitadas=l.getEncomendasSolicitadas();
        this.encomendasComTransportador=l.getEncomendasComTransportador();
        this.pedidos=l.getPedidos();
    }
   
    public List<String> getEncomendasSolicitadas() throws NullPointerException{
        return new ArrayList<>(this.encomendasSolicitadas);
    }
    
    public void setEncomendasSolicitadas (List<String> l){
        this.encomendasSolicitadas=new ArrayList<>(l);
    }
    
    public List<String> getEncomendasComTransportador(){
        return new ArrayList<>(this.encomendasComTransportador);
    }
    
    public void setEncomendasComTransportador(List<String> l){
        this.encomendasComTransportador=new ArrayList<>(l);
    }
    
    public Map<String,List<List<LinhaEncomenda>>> getPedidos(){
        return new HashMap<>(this.pedidos);
    }
    
    public void setPedidos(Map<String,List<List<LinhaEncomenda>>> l){
        this.pedidos=new HashMap<>(l);
    }
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Loja lo = (Loja) obj;
        return super.equals(lo) && this.encomendasComTransportador.equals(lo.getEncomendasComTransportador()) &&
        this.encomendasSolicitadas.equals(lo.getEncomendasSolicitadas());
    }
    
    public String toString(){
        StringBuilder sb=new StringBuilder();
        sb.append("Loja:").append("CodLoja:").append(this.getCodigo()).append(",")
                .append("Nome:").append(this.getNome()).append(",")
                .append("GPS:").append(this.getGps()).append(",")
                .append("Encomendas:").append(this.getGEncomendas()).append(",")
                .append("Encomendas Efetuadas:").append(this.getEncomendasEfetuadas()).append(",")
                .append("GPS:").append(this.getGps()).append(",")
                .append("Password:").append(this.getPassword()).append(",")
                .append("Email:").append(this.getEmail()).append(",")
                .append("Encomendas com transporte solicitado: ").append(this.encomendasSolicitadas).append(",")
                .append("Encomendas com transportador: ").append(this.encomendasComTransportador).append(".\n");
                return sb.toString();
    }
    
    public Loja clone (){
        return new Loja(this);
    }
    
    public String getCodigos(){
        StringBuilder sb=new StringBuilder();
        this.encomendasComTransportador.stream().forEach(v-> sb.append("Aceite:").append(v).append("\n"));
        return sb.toString();
    }
    
    public String paraEstado(){
        StringBuilder sb=new StringBuilder();
        sb.append("Loja:").append(super.paraEstado());
        return sb.toString();
    }
    
    public boolean encPressicaTransportador(String codEnc){
        return this.encomendasSolicitadas.contains(codEnc);
    }
    
    public void removeEncomendaSolicitada(String cod){
        this.encomendasSolicitadas.remove(cod);
    }
    
    public void addEncTransporteSolicitado(String cod){
        this.encomendasSolicitadas.add(cod);
    }
    
    public  boolean naoTemTransportador(String cod){
        return this.containsEncomenda(cod) && !(this.encomendasComTransportador.contains(cod)) && 
                !(this.encomendasSolicitadas.contains(cod));
    }
    
    public void addEncComTransportador(String cod){
        this.encomendasComTransportador.add(cod);
    }
    
    public void novoPedidoDeEncomenda(String codU,List<LinhaEncomenda> enc){
        if (this.pedidos.containsKey(codU)){
            this.pedidos.get(codU).add(enc);
        }else{
            ArrayList a = new ArrayList<>();
            a.add(enc);
            this.pedidos.put(codU,a);
        }
    }
    
    public void removePedidoEncomenda(String cod,int i){
        this.pedidos.get(cod).remove(i);
    }
    
    //(String codE, String codU, String codL,float peso, Map<String,LinhaEncomenda> prod)
    public void aceitarPedidoDeEncomenda(String codU,String cod,int i,float p,List<String> codigos){
        Map<String,LinhaEncomenda> produtos=new HashMap<>();
        List<LinhaEncomenda> l = this.pedidos.get(codU).get(i);
        for (int j=0;j<l.size();j++){
            produtos.put(codigos.get(i),l.get(i));
        }
        Encomenda enc=new EncomendaBasica(cod,codU,this.getCodigo(),p,produtos);
        this.addEncomenda(enc);
    }
    
    //pedidos tem utilizador
    public boolean pedidoTemUB(String codUB){
        return this.pedidos.containsKey(codUB);
    }
    
    //pedidos de utilizador tem index
    public boolean pedidoUBtemIndex(String codUB,int i){
        return this.pedidos.get(codUB).size()>i;
    }
    
    public List<LinhaEncomenda> getPedido (String codUB,int i){
        return this.pedidos.get(codUB).get(i);
    }
    
    public void recusarPedidoDeEncomenda(String codUB,int i){
        if (this.pedidos.get(codUB).size()==1){this.pedidos.remove(codUB);}
        else {this.pedidos.get(codUB).remove(i);}
    }
    
    //Map<String,List<List<LinhaEncomenda>>> pedidos
    public String getPedidosDeEncomendas(){
        StringBuilder sb=new StringBuilder();
        sb.append("Pedidos de encomendas:\n");
        this.pedidos.entrySet().stream().forEach(v -> sb.append("Codigo do utilizador: ").append(v.getKey()).append(": ").
        append("Encomendas: ").append(this.aux(v.getValue())));
        return sb.toString();
    }
    
    public String aux(List<List<LinhaEncomenda>> l){
        StringBuilder sb=new StringBuilder();
        for(int i=0;i<l.size();i++){
            sb.append("\n").append(i).append(") ").append(l.get(i).toString());
        }
        return sb.toString();
    }
    
    public int quantidadePedidos(){
        return this.pedidos.values().stream().mapToInt(List::size).sum();
    }
}




















