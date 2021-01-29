
public class Voluntario
{   private String codVoluntario;
    private String nome;
    private double latitude,longitude;
    private double raio;

    /**
     * COnstrutor para objetos da classe user
     */
    public Voluntario()
    {
        this.codVoluntario= new String();
        this.nome= new String();
        this.latitude=0;
        this.longitude=0;
        this.raio=0;

    }
    public Voluntario(String codvo,String name,double lat,double longi,double raio)
    {
        this.codVoluntario= codvo;
        this.nome= name;
        this.latitude=lat;
        this.longitude=longi;
        this.raio=raio;

    }
    public Voluntario(Voluntario vol)
    {
        this.codVoluntario= vol.getcod();
        this.nome= vol.getnome();
        this.latitude=vol.getlat();
        this.longitude=vol.getlong();
        this.raio=vol.getraio();

    }
    public String getcod(){
        return this.codVoluntario;
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
    public double getraio(){
        return this.raio;
    }
    public Voluntario clone(){
        return new Voluntario(this);
    }
}
