import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.io.Serializable;
import java.util.Iterator;

public abstract class Usuario implements Serializable{
    
    private String codigo;
    private String nome;
    private GPS gps;
    private String email;
    private String password;
    private GestaoEncomendas gEncomendas;//encomendas por efetuar
    private GestaoEncomendas encomendasEfetuadas;//encomendas efetuadas
    
    public Usuario(){
        this.codigo="n/a";
        this.nome="n/a";
        this.gps=new GPS();
        this.email="n/a";
        this.password=new String();
        this.gEncomendas=new GestaoEncomendas();
        this.encomendasEfetuadas=new GestaoEncomendas();
    }
    
    public Usuario (String nnome,String cod,GPS g, String nemail, String npassword, GestaoEncomendas ge,GestaoEncomendas gee){
        this.nome=nnome;
        this.codigo=cod;
        this.password=npassword;
        this.gps=g;
        this.email=nemail;
        this.setGEncomendas(ge);
        this.setEncomendasEfetuadas(gee);
    }
    
    public Usuario (Usuario u){
        this.nome=u.getNome();
        this.codigo=u.getCodigo();
        this.gps=u.getGps();
        this.email=u.getEmail();
        this.setPassword(u.getPassword());
        this.gEncomendas=u.getGEncomendas();
        this.encomendasEfetuadas=u.getEncomendasEfetuadas();
    }
    
    public String getNome(){
        return this.nome;
    }
    
    public void setNome(String n){
        this.nome=n;
    }
    
    public String getCodigo(){
        return this.codigo;
    }
    
    public void setCodigo(String cod){
        this.codigo = cod;
    }
    
    public GPS getGps(){
        return this.gps.clone();
    }
    
    public void setGps(GPS gps){
        this.gps=gps.clone();
    }
    
    public GestaoEncomendas getGEncomendas(){
        return this.gEncomendas.clone();
    }
    
    public void setGEncomendas (GestaoEncomendas ge){
        this.gEncomendas=ge.clone();
    }
    
    public GestaoEncomendas getEncomendasEfetuadas(){
        return this.encomendasEfetuadas.clone();
    }
    
    public void setEncomendasEfetuadas (GestaoEncomendas ge){
       this.encomendasEfetuadas=new GestaoEncomendas(ge);
    }
    
    public String getEmail(){
        return this.email;
    }
    
    public void setEmail(String nemail){
        this.email=nemail;
    }
    
    public String getPassword(){
        return this.password;
    }
    
    public abstract Usuario clone();
    
    public void setPassword(String npass){
        this.password=npass;
    }
           
    public boolean equals (Object o){
        if (this==o) return true;
        if (this.getClass()!=o.getClass()||o==null) return false;
        Usuario u = (Usuario) o;
        return this.gps.equals(u.getGps()) &&
                    this.nome.equals(u.getNome())&&
                    this.codigo.equals(u.getCodigo()) &&
                    this.email.equals(u.getEmail()) &&
                    this.getPassword().equals(u.getPassword())&&
                    this.gEncomendas.equals(u.getGEncomendas()) &&
                    this.encomendasEfetuadas.equals(u.getEncomendasEfetuadas());
    }
    
    public abstract String toString ();
    
    public Encomenda getEncomenda(String codEnc){
        return this.gEncomendas.getEncomenda(codEnc);
    }
    
    public String getCodUtilizador(String codE){
        return this.gEncomendas.getCodUtilizador(codE);
    }
    
    public void addEncomenda(Encomenda enc){
        this.gEncomendas.addEncomenda(enc);
    }
    
    public void addEncomendaEfetuada(EncomendaEfetuada enc){
        this.encomendasEfetuadas.addEncomenda(enc);
    }
    
    /*String para ir para o ficheiro relativo a este usuario*/  
    public String paraEstado(){
        StringBuilder sb=new StringBuilder();
        sb.append(this.getCodigo()).append(",").append(this.getNome()).append(",").append(this.getGps().toString());
        return sb.toString();
    }
    
    public String getStrEncEfetuadas(){
        StringBuilder sb=new StringBuilder();
        sb.append(this.encomendasEfetuadas.getStrEstadoEncEfe());
        return sb.toString();
    }
    
    /*retorna true se a palavra passe coincide*/ 
    public boolean passCorreta(String pass){
        return this.password.equals(pass);
    }
    
    public void encomendaEntregue(String codE,LocalDateTime t,String codT,float custo){
        this.encomendasEfetuadas.addEncomenda(new EncomendaEfetuada(this.gEncomendas.getEncomenda(codE),t,codT,custo));
        this.gEncomendas.removeEncomenda(codE);
    }
    
    public boolean containsEncomenda(String cod){
        return this.gEncomendas.contains(cod);
    }
    
    public boolean containsCodEncomenda(String cod){
        return this.gEncomendas.contains(cod) || this.encomendasEfetuadas.contains(cod);
    }
    
    public String getEstadoEncomendas(){
        return this.gEncomendas.getEstadoEncomendas();
    }
    
    public double distancia(Usuario u){
        return this.gps.distancia(u.getGps());
    }
    
    public boolean estaNoRaio(GPS gps,float raio){//u1 -> loja || u2 -> transportador

        double distancia = this.gps.distancia(gps);
        return raio >= distancia;
    }
    
    public String informacaoencomendas (){
        StringBuilder sb=new StringBuilder();
        TreeSet<EncomendaEfetuada> t=new TreeSet <EncomendaEfetuada>(new DateTimeComparatorEE());
        this.encomendasEfetuadas.getEncomendas().values().stream().map(v->(EncomendaEfetuada) v).forEach(v->t.add(v));
        Iterator<EncomendaEfetuada> itr=t.iterator();
        EncomendaEfetuada enc;
        sb.append("Informacao sobre encomendas efetudas:\n");
        while (itr.hasNext()){
            enc=itr.next();
            sb.append(enc.getTempo().toString()).append(": ").append(enc.toString()).append("\n");
        }
        return sb.toString();
    }
    
    public int getTotalEncomendasEfetuadas(){
        return this.encomendasEfetuadas.total();
    }
    
    public int getTotalEncomendas(){
        return this.gEncomendas.total();
    }
}











