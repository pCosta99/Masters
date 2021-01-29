public class Loja extends Registo{
    /** Codigo da Loja **/
    private String codLoja;
    /** Nome da Loja **/
    private String nomeLoja;
    /** Localização da Loja **/
    private Localizacao gps;

    /** Construtor nulo **/
    public Loja (){
        super();
        this.codLoja = "";
        this.nomeLoja = "";
        this.gps = new Localizacao();
    }

    /** Construtor parametrizado para a classe Loja **/
    public Loja(String ema, String pas, String codL, String nl, Localizacao ll){
        super(ema, pas);
        this.codLoja = codL;
        this.nomeLoja = nl;
        this.gps = ll;
    }

    /** Construtor de cópia **/
    public Loja (Loja l){
        super(l);
        this.codLoja = l.getCodLoja();
        this.nomeLoja = l.getNomeLoja();
        this.gps = l.getGps();
    }

    /** Retorna o código da Loja **/
    public String getCodLoja(){
        return this.codLoja;
    }

    /** Retorna o nome da Loja **/
    public String getNomeLoja(){
        return this.nomeLoja;
    }

    /** Retorna a Localização da Loja **/
    public Localizacao getGps(){
        return this.gps;
    }

    /** Define o codigo da Loja **/
    public void setCodLoja(String cl){
        this.codLoja = cl;
    }

    /** Define o nome da Loja **/
    public void setNomeLoja (String nomel){
        this.nomeLoja = nomel;
    }

    /** Define a Localização da Loja **/
    public void setGps (Localizacao lloja){
        this.gps = lloja;
    }

    /** Método que clona uma Loja **/
    public Loja clone(){
        return new Loja (this);
    }

    /** Método que devolve um boolean true caso as Lojas sejam iguais e false caso não sejam **/
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;

        Loja j = (Loja) o;
        return this.codLoja.equals(j.getCodLoja()) &&
                this.nomeLoja.equals(j.getNomeLoja()) &&
                this.gps.equals(j.getGps());
    }

    /** Método que cria uma string com a informação da Encomenda **/
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Código da Loja: ").append(this.codLoja+"\n");
        sb.append("Nome da Loja: ").append(this.nomeLoja+"\n");
        sb.append("Localização da Loja: ").append(this.gps+"\n");
        return sb.toString();
    }
}
