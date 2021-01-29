

import java.io.Serializable;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class Utilizador extends Entidade implements Serializable
{

    /** codigo do transportador que fez a ultima encomenda */
    private String codUltimoTransportador;

    /** conjunto de encomendas que já foram aceites pelo sistema mas que ainda não foram entregues. Já tem transportador associado */
    private Map<String,Encomenda> carrinho;

    /** conjunto de encomendas que já foram aceites pelo sistema mas que ainda não foram entregues e que não tem transportador associado ainda
     * Recebe o 'aceite' do ficheiro logs*/
    private Map<String,Encomenda> encAceitesLogs;

    /**
     * Construtor por omissão de utilizador
     */
    Utilizador(){
        super();
        this.codUltimoTransportador = new String();
        this.carrinho = new TreeMap<>();
        this.encAceitesLogs = new TreeMap<>();
    }

    /**
     * Construtor parametrizado de utilizador
     * @param nome nome da entidade
     * @param codigo identificador da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param registos conjunto das encomendas feitas por uma entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     */
    Utilizador(String nome,String codigo,double xGPS,double yGPS,Map<String,Encomenda> registos, String email, String pass){
        super(nome,codigo,xGPS,yGPS,registos,email,pass);
        this.codUltimoTransportador = new String();
        this.carrinho = new TreeMap<>();
        this.encAceitesLogs = new TreeMap<>();
    }

    /**
     * Construtor por copia de utilizador
     * @param u utilizador
     */
    Utilizador(Utilizador u){
        super(u.getNome(),u.getCodigo(),u.getXGPS(),u.getYGPS(),u.getRegistos(),u.getEmail(),u.getPass());
        this.codUltimoTransportador = u.getCodUltimoTransportador();
        this.carrinho = u.getCarrinho();
        this.encAceitesLogs = u.getEncAceitesLogs();
    }

    /**
     * Getter do codigo do ultimo transportador
     * @return codigo do ultimo transportador
     */
    public String getCodUltimoTransportador(){return this.codUltimoTransportador;}

    /**
     * Getter das encomendas do carrinho
     * @return encomendas do carrinho
     */
    public Map<String,Encomenda> getCarrinho(){
        Map<String,Encomenda> aux = new TreeMap<>();
        for(Map.Entry<String,Encomenda> e : this.carrinho.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    /**
     * Getter do conjunto de encomendas que já foram aceites pelo sistema mas que ainda não foram entregues e que não tem transportador associado ainda
     * @return conjunto de encomendas que já foram aceites pelo sistema mas que ainda não foram entregues e que não tem transportador associado ainda
     */
    public Map<String,Encomenda> getEncAceitesLogs(){
        Map<String,Encomenda> aux = new TreeMap<>();
        for(Map.Entry<String,Encomenda> e : this.encAceitesLogs.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    /**
     * Setter do codigo do ultimo transportador
     * @param novo novo codigo do ultimo transportador
     */
    public void setCodUltimoTransportador(String novo){this.codUltimoTransportador = novo;}


    /**
     * Metodo clone de utilizador
      * @return clone de utilizador
     */
    public Utilizador clone(){return new Utilizador(this);}

    /**
     * Metodo equals de utilizador
      * @param o Objeto
     * @return booleano
     */
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass()!=this.getClass()) return false;
        Utilizador e = (Utilizador) o;
        return super.getCodigo().equals(e.getCodigo()) &&
                super.getNome().equals(e.getNome()) &&
                super.getXGPS() == e.getXGPS() &&
                super.getYGPS() == e.getYGPS() &&
                super.getRegistos().equals(e.getRegistos()) &&
                super.getEmail().equals(e.getEmail()) &&
                super.getPass().equals(e.getPass()) &&
                this.codUltimoTransportador.equals(e.getCodUltimoTransportador()) &&
                this.carrinho.equals(e.getCarrinho()) &&
                this.encAceitesLogs.equals((e.getEncAceitesLogs()));
    }

    /**
     * Metodo toString de utilizador
      * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append("\nUtilizador:\n").append(super.toString()).append(" | Encomendas no carrinho: ").append(this.carrinho);
        return s.toString();
    }

    /**
     * Metodo qeu retorna uma String com o codigo, nome e coordenadas de utilizador
     * @return String
     */
    public String toStringCSV(){
        StringBuilder s = new StringBuilder();
        s.append("Utilizador:").append(super.getCodigo()).append(",").append(super.getNome()).append(",").append(super.getXGPS()).append(",").append(super.getYGPS()).append("\n");
        return s.toString();
    }


    /**
     * Metodo que devolve o numero de encomenda feitas por um utilizador
     * @return numero de encomenda feitas por um utilizador
     */
    public int getNumeroEncomendas(){return super.getRegistos().size() + this.carrinho.size() + this.encAceitesLogs.size();}


    /**
     * Metodo que insere uma encomenda no carrinho
     * @param e encomenda a inserir
     */
    public void insereCarrinho(Encomenda e){this.carrinho.put(e.getCodEncomenda(),e.clone());}


    /**
     * Metodo que remove uma encomenda no carrinho
     * @param e encomenda a remover
     */
    public void removeCarrinho(Encomenda e){this.carrinho.remove(e.getCodEncomenda());}


    /**
     * Metodo que insere uma encomenda no encAceiteLogs
     * @param e encomenda a inserir
     */
    public void insereEncAceitesLogs(Encomenda e){this.encAceitesLogs.put(e.getCodEncomenda(),e.clone());}


    /**
     * Metodo que remove uma encomenda no encAceiteLogs
     * @param e encomenda a remover
     */
    public void removeEncAceitesLogs(Encomenda e){this.encAceitesLogs.remove(e.getCodEncomenda());}


    /**
     * Metodo que devolve os codigos de todas as encomendas do encAceiteLogs
     * @return codigos de todas as encomendas do encAceiteLog
     * @throws ConjuntoVazioException
     */
    public Set<String> codEncs() throws ConjuntoVazioException{
        if(this.encAceitesLogs.size()<=0) throw new ConjuntoVazioException();
        else
            return this.encAceitesLogs.keySet();
    }
}