
/**
 * Escreva a descrição da classe lojas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class lojas
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private String codl;
    private String nome;
    private GPS GPS;

    /**
     * COnstrutor para objetos da classe lojas
     */
    public lojas(String c,String n,GPS s)
    {
        this.codl=c;
        this.nome=n;
        this.GPS=s;
    }
    public lojas(lojas x)
    {
        this.codl=x.getcodl();
        this.nome=x.getnome();
        this.GPS=x.getGPS();
    }
    
    public String getnome(){
        return this.nome;
    }
    public String getcodl(){
        return this.codl;
    }
    public GPS getGPS(){
        return this.GPS;
    }
    
    public void setcodl(String x){
        this.codl=x;
    }
    public void setnome(String x) {
        this.nome=x;
    }
    public void setGPS(GPS x){
        this.GPS=x;
    }
    
    public lojas clone(){
        return new lojas(this);
    }
    public boolean equals(Object obj){
        if(obj==this)return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        lojas x = (lojas) obj;
        return x.getnome().equals(this.nome) &&
               x.getGPS().equals(this.GPS) &&
               x.getcodl()==this.codl;
    }
}
