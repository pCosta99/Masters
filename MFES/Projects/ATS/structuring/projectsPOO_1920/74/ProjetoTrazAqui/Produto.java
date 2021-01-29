import java.io.Serializable;
public class Produto implements Serializable{
    private double preco;
    private String desc;
    private String codigo;
    private boolean medico;
    /**
     * Construtor por omissao da classe
     */
    public Produto(){
        this.preco = 0.0;
        this.desc = this.codigo = "";
        this.medico = false;
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros String cod,String d, double p
     */ 
    public Produto(String cod,String d, double p){
        this.preco = p;
        this.desc = d;
        this.codigo = cod;
        this.medico = false;
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros String cod,String d, double p,boolean m
     */ 
    public Produto(String cod,String d, double p,boolean m){
        this.preco = p;
        this.desc = d;
        this.codigo = cod;
        this.medico = m;
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
    public Produto(Produto p){
        this.preco = p.getPreco();
        this.desc = p.getDesc();
        this.codigo = p.getCodigo();
        this.medico = p.getMedico();
    }
    /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Preco: ").append(this.preco).append("\n");
        sb.append("Descrição: ").append(this.desc).append("\n");
        sb.append("Código: ").append(this.codigo).append("\n");
        sb.append("Produto médico: ").append(this.medico).append("\n");
        return sb.toString();
    }

     /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || (this.getClass() != o.getClass()))
            return false;
        Produto p = (Produto) o;
        return this.preco == p.getPreco() && this.desc.equals(p.getDesc()) && this.codigo.equals(p.getCodigo()) && this.medico == p.getMedico();
    }
      /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public Produto clone(){
        return new Produto(this);
    }
    /**
     * Atualiza a String desc da classe
     * @param s novo desc da classe
     */
    public void setDesc(String s){
        this.desc = s;
    }
    /**
     * Atualiza a String codigo da classe
     * @param c novo codigo da classe
     */
    public void setCodigo(String c){
        this.codigo = c;
    }
    /**
     * Atualiza o double preco da classe
     * @param p novo preco da classe
     */
    public void setPreco(double p){
        this.preco = p;
    }
    /**
     * Devolve o double preco da classe
     * @return double preco
     */
    public double getPreco(){
        return this.preco;
    }
    /**
     * Devolve a String codigo da classe
     * @return String codigo
     */
    public String getCodigo(){
        return this.codigo;
    }
    /**
     * Devolve a String desc da classe
     * @return String desc
     */
    public String getDesc(){
        return this.desc;
    }
    /**
     * Devolve o boolean medico da classe
     * @return boolean medico
     */
    public boolean getMedico(){
        return this.medico;
    }
     /**
     * Atualiza o boolean medico da classe
     * @param m novo medico da classe
     */
    public void setMedico(boolean m){
        this.medico = m;
    }
    
}
