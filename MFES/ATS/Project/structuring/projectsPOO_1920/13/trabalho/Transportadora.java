

public class Transportadora
{private String codTransportadora;
    private String nome;
    private double latitude,longitude;
    private double raio;
    private double priceperkm;
    
    /**
     * COnstrutor para objetos da classe user
     */
    public Transportadora()
    {
        this.codTransportadora= new String();
        this.nome= new String();
        this.latitude=0;
        this.longitude=0;
        this.raio=0;
        this.priceperkm=0;
    }
    public Transportadora(String codtrans,String name,double lat,double longi,double raio,double preco)
    {
        this.codTransportadora= codtrans;
        this.nome= name;
        this.latitude=lat;
        this.longitude=longi;
        this.raio=raio;
        this.priceperkm=preco;
    }
    public Transportadora(Transportadora trans)
    {
        this.codTransportadora= trans.getcod();
        this.nome= trans.getnome();
        this.latitude=trans.getlat();
        this.longitude=trans.getlong();
        this.raio=trans.getraio();
        this.priceperkm=trans.getpreco();
    }
    public String getcod(){
        return this.codTransportadora;
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
    public double getpreco(){
        return this.priceperkm;
    }
    public Transportadora clone(){
        return new Transportadora(this);
    }
}
