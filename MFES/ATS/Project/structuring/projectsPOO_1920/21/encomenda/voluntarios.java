
/**
 * Escreva a descrição da classe voluntarios aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class voluntarios
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private String codv;
    private GPS GPS;
    private String nome;
    private float raio;
    private boolean d;
    
    public void setd(boolean x){
        this.d=x;
    }

    /**
     * COnstrutor para objetos da classe voluntarios
     */
    public voluntarios(String x,float r,GPS s, String c)
    {
        this.codv = x;
        this.raio=r;
        this.GPS=s;
        this.nome=c;
        this.d=false;
    }
    public voluntarios(voluntarios x){
        this.codv = x.getcodv();
        this.raio=x.getraio();
        this.GPS=x.getGPS();
        this.nome=x.getnome();
        this.d=false;
    }
    
    public String getcodv(){
        return this.codv;
    }
    public float getraio(){
        return this.raio;
    }
    public String getnome(){
        return this.nome;
    }
    public GPS getGPS(){
        return this.GPS;
    }
    
    public void setcodv(String x){
        this.codv=x;
    }
    public void setraio(float x){
        this.raio=x;
    }
    public void setnome(String x){
        this.nome=x;
    }
    public void setGPS(GPS x){
        this.GPS=x;
    }
    
    public voluntarios clone(){
        return new voluntarios(this);
    }
    public boolean equals(Object o)
    {
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        voluntarios s= (voluntarios) o;
        return s.getnome().equals(this.nome) &&
               s.getcodv()==this.codv &&
               s.getraio()==this.raio &&
               s.getGPS().equals(this.GPS);
    }
}
