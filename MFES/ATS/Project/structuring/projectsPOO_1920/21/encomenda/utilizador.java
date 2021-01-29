
/**
 * Escreva a descrição da classe utilizador aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class utilizador
{
    
    private String codutilizador;
    private String nome;
    private GPS GPS;

    /**
     * COnstrutor para objetos da classe utilizador
     */
    public utilizador()
    {
        // inicializa variáveis de instância
        this.codutilizador = "n/a";
        this.nome="/na";
        
    }
    public utilizador(String x, String y,GPS z)
    {
        this.codutilizador=x;
        this.nome=y;
        this.GPS=z;
    }
    public utilizador(utilizador x)
    {
        this.codutilizador=x.getCodUtilizador();
        this.nome=x.getnome();
        this.GPS=x.getGPS();
    }
    
    public String getnome(){
        return this.nome;
    }
    public String getCodUtilizador(){
        return this.codutilizador;
    }
    public GPS getGPS(){
        return this.GPS;
    }
    
    public void setnome(String x){
        this.nome=x;
    }
    public void setCodUtilizador(String x){
        this.codutilizador=x;
    }
    public void setGPS(GPS x){
        this.GPS=x;
    }
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        utilizador le = (utilizador) obj;
        return this.codutilizador==le.getCodUtilizador() &&
               this.GPS.equals(le.getGPS()) &&
               this.nome.equals(le.getnome());
    }
    public utilizador clone(){
        return new utilizador(this);
    }
}



    