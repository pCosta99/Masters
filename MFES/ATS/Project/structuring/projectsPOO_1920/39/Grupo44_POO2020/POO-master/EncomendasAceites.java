public class EncomendasAceites {
    /** Codigo do produto **/
    private String codEncomenda;

    /** Construtor nulo **/
    public EncomendasAceites(){
        this.codEncomenda = "";
    }

    /** Construtor parametrizado para a classe EncomendasAceites **/
    public EncomendasAceites(String cE){
        this.codEncomenda = cE;
    }

    /** Construtor de cópia **/
    public EncomendasAceites(EncomendasAceites eA){
        this.codEncomenda = eA.getCodEncomenda();
    }

    /** Retorna o Codigo da Encomenda **/
    public String getCodEncomenda(){
        return this.codEncomenda;
    }

    /** Define o codigo da Encomenda **/
    public void setCodEncomenda(String cdE){
        this.codEncomenda = cdE;
    }

    /** Método que clona as Encomendas Aceites **/
    public EncomendasAceites clone(){
        return new EncomendasAceites(this);
    }

    /** Método que devolve um boolean true caso as Encomendas Aceites sejam iguais e false caso não sejam **/
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;

        EncomendasAceites ea = (EncomendasAceites) o;
        return this.codEncomenda.equals(ea.getCodEncomenda());
    }

    /** Método que cria uma string com a informação da Encomenda **/
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda Aceite: ").append(this.codEncomenda+"\n");
        return sb.toString();
    }

}
