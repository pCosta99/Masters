public class Utilizador
{
    private String codUtilizador;
    private String nome;
    private double latitude,longitude;
    
    /**
     * COnstrutor para objetos da classe Utilizador
     */
    public Utilizador()
    {
        this.codUtilizador= new String();
        this.nome= new String();
        this.latitude=0;
        this.longitude=0;
        
    }
    public Utilizador(String codUtilizador,String name,double lat,double longi)
    {
        this.codUtilizador= codUtilizador;
        this.nome= name;
        this.latitude=lat;
        this.longitude=longi;
        
    }
    public Utilizador(Utilizador user)
    {
        this.codUtilizador= user.getcod();
        this.nome= user.getnome();
        this.latitude=user.getlat();
        this.longitude=user.getlong();
        
    }
    public String getcod(){
        return this.codUtilizador;
    }
    public String getnome(){
        return this.nome;
    }
    public double getlat(){
        return this.latitude;
    }
    public double getlong(){
        return this.longitude;
    }
    public Utilizador clone(){
        return new Utilizador(this);
    }
}    