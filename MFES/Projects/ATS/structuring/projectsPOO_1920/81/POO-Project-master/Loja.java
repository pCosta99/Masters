import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
/**
 * Write a description of class loja here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Loja extends Dados implements Serializable{
    private String codLoja;
    private String email;
    private String password;
    private String nome;
    private double gpsx;
    private double gpsy;
    private int nif;
    private List<Produto> produtos;
    private List<Encomenda> encomendas;
    
    /**
     * Construtor para objetos da classe Loja (por omissao)
     */
    public Loja(){
        this.codLoja = new String();
        this.nome = "n/a";
        this.email = "n/a";
        this.password = "n/a";
        this.gpsx = 0;
        this.gpsy = 0;
        this.nif = 0;
        this.produtos = new ArrayList<>();
        this.encomendas = new ArrayList<>(); 
    }
    /**
     * Construtor para objetos da classe Loja (parametrizado).
     * 
     * @param  cod  o codigo da loja
     * @param  em   o email
     * @param  nm   o nome
     * @param  pw   a password
     * @param  x    o gpsX
     * @param  y    o gpsY
     * @param  nif  o nif
     * @param  en   as encomendas
     * @param  p   os produtos
     */
    public Loja(String cod, String em, String nm, String pw, double x, double y, int nif,  List<Produto> p, List<Encomenda> en){
        this.setCod(cod);
        this.setEmail(em);
        this.setNome(nm);
        this.setPassword(pw);
        this.setGPSx(x);
        this.setGPSy(y);
        this.setNif(nif);
        this.setProdutos(p);
        this.setEncomendas(en);
    }
    /**
     * Construtor para objetos da classe Loja (de copia)
     * 
     * @param  l   a Loja
     */
    public Loja(Loja l){
        this.codLoja = l.getCod();
        this.email = l.getEmail();
        this.nome = l.getNome();
        this.password = l.getPassword();
        this.gpsx = l.getGPSx();
        this.gpsy = l.getGPSy();
        this.nif = l.getNif();
        this.produtos = l.getProdutos();
        this.encomendas = l.getEncomendas();
    }
    /*Getters*/
    public String getCod(){ return this.codLoja;}
    public String getEmail(){return this.email;}
    public String getNome(){return this.nome;}
    public String getPassword(){return this.password;}
    public double getGPSx(){ return this.gpsx;}
    public double getGPSy(){return this.gpsy;}
    public int getNif(){ return this.nif;}
    public List<Produto> getProdutos(){
        List<Produto> aux = new ArrayList<>();
        for(Produto e : this.produtos)
            aux.add(e.clone());
        return aux;
    } 
    public List<Encomenda> getEncomendas(){
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda e : this.encomendas)
        aux.add(e.clone());
        return aux;
    }
    
    /*Setters*/
    public void setCod(String c ){this.codLoja = c;}
    public void setEmail(String em){this.email = em;}
    public void setNome(String n){this.nome = n;}
    public void setPassword(String pd){this.password = pd;}
    public void setGPSx(double x){this.gpsx = x;}
    public void setGPSy(double y){this.gpsy = y;}
    public void setNif(int nif){ this.nif = nif;}
    public void setEncomendas(List<Encomenda> en){
        this.encomendas = new ArrayList<>();
        for(Encomenda e : en)
            this.encomendas.add(e.clone());
    }
    public void setProdutos(List<Produto> pr){
        this.produtos = new ArrayList<>();
        for(Produto p : pr)
        this.produtos.add(p.clone());
    }
    /**
     * Metodo que duplica a loja
     * 
     * @return     o clone da loja
     */
    public Loja clone(){
       return new Loja(this);
    }
    
    /**
     * Metodo que verifica se duas lojas sao iguais
     * 
     * @param  o   o objeto a comparar
     * 
     * @return     o resultado da comparacao dos objetos
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        else{
            Loja l = (Loja) o;
            return(l.getCod().equals(this.getCod())
                   && l.getEmail().equals(this.getEmail())
                   && l.getNome().equals(this.getNome())
                   && l.getPassword().equals(this.getPassword())
                   && l.getGPSx()==(this.getGPSx())
                   && l.getGPSy()==(this.getGPSy())
                   && l.getNif()==(this.getNif())
                   && l.getProdutos()==(this.getProdutos())
                   && l.getEncomendas()==(this.getEncomendas()));
        }
    }
    
    /**
     * Metodo que converte uma loja para uma string
     * 
     * @return     a loja em string
     */
    public String toString(){
        String aux = "Codigo da Loja: " + this.codLoja + ";\n" 
                     + "Email da Empresa:" + this.email + ";\n"
                     + "Nome da Empresa: " + this.nome + ";\n"
                     + "Password:" + this.password + ";\n"
                     + "Destino X " + this.gpsx + ";\n"
                     + "Destino Y: " + this.gpsy + ";\n"
                     + "Nif: " + this.nif + ";\n"
                     + "Encomendas: \n";
        for(Encomenda e : this.encomendas)
            aux += e.toString() + "\n";
        aux += "Produto: \n";
        for(Produto p : this.produtos)
            aux += p.toString() + "\n";
        return aux;
    }
    /**
     * Metodo que devolve o codigo de hash para uma loja
     * 
     * @return     o hashcode
     */
    public int hashCode(){
        int hash = 7; 
        hash = 31 * hash + codLoja.hashCode();
        hash = 31 * hash + email.hashCode();
        hash = 31 * hash + nome.hashCode();
        hash = 31 * hash + password.hashCode();
        long aux = Double.doubleToLongBits(gpsx);
        hash = 31 * hash + (int)(aux ^ (aux >>> 32));
        long aux1 = Double.doubleToLongBits(gpsy);
        hash = 31 * hash + (int)(aux1 ^ (aux1 >>> 32)); 
        hash = 31 * hash + nif;
        for(Encomenda e : this.encomendas)
            hash = 31 * hash + e.hashCode();
        for(Produto p : this.produtos)
            hash = 31 * hash + p.hashCode();
        return hash;       
    }
    
    /**
     * Metodo que implementa a ordem natural de comparacao de instancias de loja
     */
    public int compareTo(Loja l){
        return l.getEmail().compareTo(this.email);
    }
    
    /**
     * Metodo que verifica se o email e a password dados sao os da loja
     * 
     * @param  mail   o email a comparar
     * @param  pass   a password a comparar
     * 
     * @return  o resultado da comparacao
     */
    public boolean dadosValidosLoja(String mail, String pass){
        return(this.email.equals(mail) && this.password.equals(pass));
    }
    
    /**
     * Metodo que adiciona um produto ao conjunto de produtos de uma loja
     * 
     * @param  p   o produto a adicionar
     */
    public void adicionaProduto(Produto p){
        for(Produto a : this.produtos){
            if(a.getCodProduto().equals(p.getCodProduto())){
                this.produtos.remove(a);
                break;
            }
        }
        this.produtos.add(p.clone());
    }
    /**
     * Metodo que insere uma Encomenda na lista de encomendas
     * 
     * @param  e   encomenda a ser inserida
     */
    public void insereEncomenda(Encomenda e){
        this.encomendas.add(e.clone());
    }
    
    /**
     * Metodo que aceita uma Encomenda na lista de encomendas
     * 
     * @param  e   encomenda a ser inserida
     */
    public void aceitaEncomenda(EncomendaAceite e){
        this.encomendas.add(e.clone());
    }
    
    /**
     * Metodo que verifica se ha alguma Encomenda Pendente
     * 
     */
    public boolean verificaEncomendaPendente(){
        for(Encomenda e : this.getEncomendas()){
            if(e.getEstado() == 1)
                return true;
        }
        return false;
    }
    
    /**
     * Metodo que devolve a Encomenda pendente
     * 
     * @param  e   Encomenda a ser inserida
     */
    public Encomenda encomendaPendente(){
        Encomenda b = null;
        for(Encomenda e : this.getEncomendas()){
            if(e.getEstado() == 1)
                b = e;
        }
        return b;
    }
    
    //public int pessoasEmfilaEspera
    
}
