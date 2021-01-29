import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
/**
 * Write a description of class utilizador here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Utilizador extends Dados implements Serializable
{
    private String codUtilizador;
    private String email;
    private String nome;
    private String password;
    private String morada;
    private LocalDate ddn;
    private int nif;
    private double gpsx;
    private double gpsy;
    private double classificacao;
    private List <Encomenda> encomendas;
    
    public Utilizador (){
        this.codUtilizador = new String();
        this.email = "n/a";
        this.nome = "n/a";
        this.password = "n/a";
        this.morada = "n/a";
        this.ddn = LocalDate.now();
        this.nif = 0;
        this.gpsx = 0.0;
        this.gpsy = 0.0;
        this.classificacao = 0;
        this.encomendas = new ArrayList <>();
    }
    
    public Utilizador(String cod, String em, String nm, String pw, String mr, LocalDate data, int n, double x, double y, double cla,List<Encomenda> e){
        this.setCodUtilizador(cod);
        this.setEmail(em);
        this.setNome(nm);
        this.setPassword(pw);
        this.setMorada(mr);
        this.setDDN(data);
        this.setNif(n);
        this.setGPSx(x);
        this.setGPSy(y);
        this.setClassificacao(cla);
        this.setEncomendas(e);
    }
    
    public Utilizador(Utilizador u){
        this.codUtilizador = u.getCod(); 
        this.email = u.getEmail();
        this.nome = u.getNome();
        this.password = u.getPassword();
        this.morada = u.getMorada();
        this.ddn = u.getDDN();
        this.nif = u.getNif();
        this.gpsx = u.getGPSx();
        this.gpsy = u.getGPSy();
        this.classificacao = u.getClassificacao();
        this.encomendas = u.getEncomendas();
    }
    
    public String getCod(){
        return this.codUtilizador;
    }
    public String getEmail(){
        return this.email;
    }
    public String getNome(){
        return this.nome;
    }
    public String getPassword(){
        return this.password;
    }
    public String getMorada(){
        return this.morada;
    }
    public LocalDate getDDN(){
        return this.ddn;
    }
    public int getNif(){
        return this.nif;
    }
    public double getGPSx(){ 
        return this.gpsx;
    }
    public double getGPSy(){
        return this.gpsy;
    }
    public double getClassificacao(){
        return this.classificacao;
    }
    public List<Encomenda> getEncomendas(){
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda e : this.encomendas)
            aux.add(e.clone());
        return aux;
    }
   
    public void setCodUtilizador(String c ){
        this.codUtilizador = c;
    }
    public void setEmail(String em){
        this.email = em;
    }
    public void setNome(String nm){
        this.nome = nm;
    }
    public void setMorada(String mr){
        this.morada = mr;
    }
    public void setPassword(String pd){
        this.password = pd;
    }
    public void setDDN(LocalDate data){
        this.ddn = data;
    }
    public void setNif(int n){
        this.nif = n;
    }
    public void setGPSx(double x){
        this.gpsx = x;
    }
    public void setGPSy(double y){
        this.gpsy = y;
    }
    public void setClassificacao(double cla){
        this.classificacao = cla;
    }
    public void setEncomendas(List<Encomenda> en){
        this.encomendas = new ArrayList<>();
        for(Encomenda e : en)
            this.encomendas.add(e.clone());
    }
    
    public Utilizador clone(){
       return new Utilizador(this);
    }
    
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        else{
            Utilizador u = (Utilizador) o;
            return(u.getCod().equals(this.getCod())
                   && u.getEmail().equals(this.getEmail())
                   && u.getNome().equals(this.getNome())
                   && u.getPassword().equals(this.getPassword())
                   && u.getMorada().equals(this.getMorada())
                   && u.getDDN().equals(this.getDDN())
                   && u.getNif() == this.getNif()
                   && u.getGPSx() == this.getGPSx()
                   && u.getGPSy() == this.getGPSy()
                   && u.getClassificacao() == this.getClassificacao()
                   && u.getEncomendas().equals(this.encomendas));
        }
    }
    public String toString(){
        String aux = "Codigo do Utilizador: " + this.codUtilizador + ";\n" 
                     +"Email: " + this.email + ";\n" 
                     + "Nome: " + this.nome + ";\n"
                     + "Password: " + this.password + ";\n"
                     + "Morada: " + this.morada + ";\n"
                     + "Data de Nascimento: " + this.ddn.toString() + ";\n"
                     + "Nif: " + this.nif + ";\n"
                     + "Coordenada X: " + this.gpsx + ";\n"
                     + "Coordenada Y: " + this.gpsy + ";\n"
                     + "Classificacao: " + this.classificacao + ";\n"
                     + "Encomendas: \n";
        for(Encomenda e : this.encomendas)
            aux += e.toString() + "\n";
        return aux;
    }
   
    
    /**
     * Metodo que devolve o codigo de hash para um utilizador
     * 
     * @return     o hashcode
     */
    public int hashCode(){
        int hash = 7; 
        hash = 31 * hash + nome.hashCode();
        hash = 31 * hash + email.hashCode();
        hash = 31 * hash + nome.hashCode();
        hash = 31 * hash + password.hashCode();
        hash = 31 * hash + morada.hashCode();
        hash = 31 * hash + ddn.hashCode();
        hash = 31 * hash + nif;
        long aux = Double.doubleToLongBits(gpsx);
        hash = 31 * hash + (int)(aux ^ (aux >>> 32));
        long aux1 = Double.doubleToLongBits(gpsy);
        hash = 31 * hash + (int)(aux1 ^ (aux1 >>> 32));
        long aux2 = Double.doubleToLongBits(classificacao);
        hash = 31 * hash + (int)(aux ^ (aux >>> 32));
        for(Encomenda e : this.encomendas)
            hash = 31 * hash + e.hashCode();
        return hash;
    }
    
     /**
     * Metodo que implementa a ordem natural de comparacao de instancias de Utilizador
     */
    public int compareTo(Utilizador u){
        return u.getEmail().compareTo(this.email);
    }
    
    /**
     * Metodo que devolve o numero de encomendas de um utilizador
     * 
     * @return     o utilizador em string
     */
    public String toString_top10(){
        String aux = "O Utilizador " + this.email + " usou o sistema " + this.getEncomendas().size() + " vezes\n";
        return aux;
    }
    
    /**
     * Metodo que verifica se o email e a password dados sao os do utilizador
     * 
     * @param  ema   o email a comparar
     * @param  pass   a password a comparar
     * 
     * @return  o resultado da comparacao
     */
    public boolean dadosValidosUtilizador(String cod,String ema, String pass){
        return(this.codUtilizador.equals(cod) && this.email.equals(ema) && this.password.equals(pass));
    }
    
    /**
     * Metodo que insere uma encomenda na lista de encomendas
     * 
     * @param  e   encomenda a ser inserida
     */
    public void insereEncomenda(Encomenda e){
        this.encomendas.add(e.clone());
    }
    
    /**
     * Metodo que insere uma encomenda na lista de encomendas
     * 
     */
    public boolean verificaEncomendaPendente(){
        for(Encomenda e : this.getEncomendas()){
            if(e.getEstado() == 2)
                return true;
        }
        return false;
    }
    
    /**
     * Metodo que insere uma encomenda na lista de encomendas
     * 
     */
    public boolean verificaEncomendaRejeitada(){
        for(Encomenda e : this.getEncomendas()){
            if(e.getEstado() == 3)
                return true;
        }
        return false;
    }
    
    /**
     * Metodo que devolve a Encomenda pendente
     * 
     * @param  e  Encomenda a ser inserida
     */
    public Encomenda encomendaPendente(){
        Encomenda b = null;
        for(Encomenda a : this.getEncomendas()){
            if(a.getEstado() == 2)
                b = a;
        }
        return b;
    }
    
    /**
     * Metodo que devolve a encomenda pendente 3
     * 
     * @param  e   encomenda a ser inserida
     */
    public Encomenda encomendaPendente3(){
        Encomenda b = null;
        for(Encomenda a : this.getEncomendas()){
            if(a.getEstado() == 3)
                b = a;
        }
        return b;
    }
    /**
     * Metodo que altera a classificacao
     */
    public void alteraClassificacao(double cla){
        double classi = this.classificacao * this.encomendas.size();
        classi = (classi + cla)/(this.encomendas.size() + 1);
        setClassificacao(classi);
    }
}






