import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

public abstract class Entidade implements Serializable
{
    /** e-mail da entidade */
    private String email;

    /** palavra-passe da entidade */
    private String password;

    /** identificador da entidade */
    private String codigo;

    /** nome da entidade */
    private String nome;

    /** coordenada x da entidade */
    private double xGPS;

    /** coordenada y da entidade */
    private double yGPS;

    /** conjunto das encomendas feitas por uma entidade */
    private Map<String,Encomenda> registos;

    /**
     * Construtor por omissão de Entidade
     */
    Entidade(){
        this.nome = new String();
        this.codigo = new String();
        this.xGPS = 0;
        this.yGPS = 0;
        this.registos = new TreeMap<>();
        this.email = new String();
        this.password = new String();
    }

    /**
     * Construtor parametrizado de Entidade
     * @param nome nome da entidade
     * @param codigo identificador da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param registos conjunto das encomendas feitas por uma entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     */
    Entidade(String nome,String codigo,double xGPS,double yGPS,Map<String,Encomenda> registos, String email, String pass){
        this.nome = nome;
        this.codigo = codigo;
        this.xGPS = xGPS;
        this.yGPS = yGPS;
        this.registos = registos;
        this.password = pass;
        this.email = email;
    }


    /**
     * Getter do nome da entidade
     * @return nome da entidade
     */
    public String getNome(){return this.nome;}

    /**
     * Getter do identificador da entidade
     * @return identificador da entidade
     */
    public String getCodigo(){return this.codigo;}

    /**
     * Getter da coordenada x da entidade
     * @return coordenada x da entidade
     */
    public double getXGPS(){return this.xGPS;}

    /**
     * Getter da coordenada y da entidade
     * @return coordenada y da entidade
     */
    public double getYGPS(){return this.yGPS;}

    /**
     * Getter do conjunto das encomendas feitas por uma entidade
     * @return conjunto das encomendas feitas por uma entidade
     */
    public Map<String,Encomenda> getRegistos(){
        Map<String,Encomenda> aux = new HashMap<>();
        for(Map.Entry<String,Encomenda> l : this.registos.entrySet())
            aux.put(l.getKey(),l.getValue().clone());
        return aux;
    }

    /**
     * Getter do email da entidade
     * @return email da entidade
     */
    public String getEmail(){return this.email;}

    /**
     * Getter do palavra-passe da entidade
     * @return palavra-passe da entidade
     */
    public String getPass(){return this.password;}

    /**
     * Setter do nome da entidade
     * @param novo novo nome da entidade
     */
    public void setNome(String novo){this.nome = novo;}

    /**
     * Setter do identificador da entidade
     * @param novo novo identificador da entidade
     */
    public void setCodigo(String novo){this.codigo = novo;}

    /**
     * Setter da coordenada x da entidade
     * @param novo nova coordenada x da entidade
     */
    public void setXGPS(double novo){this.xGPS = novo;}

    /**
     * Setter da coordenada y da entidade
     * @param novo nova coordenada y da entidade
     */
    public void setYGPS(double novo){this.yGPS= novo;}

    /**
     * Setter do conjunto das encomendas feitas por uma entidade
     * @param novo novo conjunto das encomendas feitas por uma entidade
     */
    public void setRegistos(Map<String,Encomenda> novo){
        this.registos = new HashMap<>();
        for(Map.Entry<String,Encomenda> l : novo.entrySet())
            this.registos.put(l.getKey(),l.getValue().clone());
    }

    /**
     * Setter do email da entidade
     * @param email novo email da entidade
     */
    public void setEmail(String email){this.email = email;}

    /**
     * Setter da palavra-passe da entidade
     * @param password nova palavra-passe da entidade
     */
    public void setPass(String password){this.password = password;}


    /**
     * Metodo toString de Entidade
     * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append("Nome: ").append(this.nome).append(" | Codigo: ").append(this.codigo)
                .append(" | Email: ").append(this.email).append(" | Passe : ").append(this.password).append(" | Coordenadas: X: ").append(this.xGPS)
                .append(" e Y: ").append(this.yGPS).append(" | Encomendas feitas ate ao momento: ")
                .append(this.registos);
        return s.toString();
    }


    /**
     * Metodo que devolve uma String com o codigo e nome da entidade
     * @return String
     */
    public String toStringEcra(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.codigo).append(" - ").append(this.nome);
        return sb.toString();
    }


    /**
     * Metodo abstrato equals
     * @param o Objeto
     * @return booleano
     */
    public abstract boolean equals(Object o);


    /**
     * Metodo abstrato clone
     * @return clone da entidade
     */
    public abstract Entidade clone();


    /**
     * Inserir uma encomenda nos registos de uma entidade
     * @param e encomenda a inserir
     */
    public void insereEnc(Encomenda e){this.registos.put(e.getCodEncomenda(),e.clone());}

    /**
     * Metodo toStringCSV abstrato
     * @return String
     */
    public abstract String toStringCSV();

}