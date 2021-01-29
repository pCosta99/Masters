

/**
 * Escreva a descrição da classe transportadores aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class transportadores
{
    // variáveis de instância 
    private String codempresa;
    private String nome;
    private GPS GPS;
    private int nif;
    private float raio;
    private float precopk;
    private boolean d;
    
    public void setd(boolean x){
        this.d=x;
    }
    public boolean getd(){
        return this.d;
    }
    

    /**
     * COnstrutor para objetos da classe transportadores
     */
    public transportadores(String x, String n,GPS c,int nif,float raio,float p)
    {
        this.codempresa=x;
        this.nome=n;
        this.GPS=c;
        this.nif= nif;
        this.raio=raio;
        this.precopk=p;
        this.d=false;
    }
    public transportadores (transportadores x)
    {
        this.codempresa=x.getcodE();
        this.nome=x.getnome();
        this.raio=x.getraio();
        this.precopk=x.getpreco();
        this.nif=x.getnif();
        this.GPS=x.getGPS();
        this.d=false;
    }
    
    public String getnome(){
        return this.nome;
    }
    public String getcodE(){
        return this.codempresa;
    }
    public float getraio(){
        return this.raio;
    }
    public GPS getGPS(){
        return this.GPS; 
    }
    public int getnif(){
        return this.nif;
    }
    public float getpreco(){
        return this.precopk;
    }
    
    public void setnome(String x){
        this.nome=x;
    }
    public void setGPS(GPS x){
        this.GPS = x;
    }
    public void setcodE(String x){
        this.codempresa=x;
    }
    public void setnif(int x){
        this.nif=x;
    }
    public void setprecopk(float x){
        this.precopk=x;
    }
    public void setraio(float x){
        this.raio=x;
    }
    
    public transportadores clone(){
        return new transportadores(this);
    }
    public boolean equals(Object o)
    {
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass())return false;
        transportadores x= (transportadores) o;
        return x.getnome().equals(this.nome)&&
               x.getcodE()==this.codempresa &&
               x.getGPS().equals(this.GPS) &&
               x.getnif()==this.nif &&
               x.getraio()==this.raio;
    }
}
