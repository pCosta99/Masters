public class Proposta{
    private String codT;
    private float preco;
    
    public Proposta(){
        this.codT=null;
        this.preco=0;
    }
    
    public Proposta(String c, float p){
        this.codT=c;
        this.preco=p;
    }
    
    public Proposta(Proposta p){
        this.codT=p.getCod();
        this.preco=p.getP();
    }
    
    public void setCod(String c){
        this.codT=c;
    }
    
    public String getCod(){
        return this.codT;
    }
    
    public void setP(float p){
        this.preco=p;
    }
    
    public float getP(){
        return this.preco;
    }
    
    public boolean equals (Object o){
        if (o==this) return true;
        if (o==null || !(o.getClass().equals(this.getClass()))) return false;
        Proposta p=(Proposta) o;
        return this.codT.equals(p.getCod()) && this.preco==p.getP();
    }
    
    public Proposta clone(){
        return new Proposta(this);
    }
    
    public String toString (){
        StringBuilder sb=new StringBuilder();
        sb.append("Codigo transportador: ").append(this.codT).append("; Preco: ").append(this.preco);
        return sb.toString();
    }
}
