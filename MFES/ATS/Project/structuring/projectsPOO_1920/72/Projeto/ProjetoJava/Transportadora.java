import Interfaces.EntregadorCobrador;
import ClassesAux.GPS;
import java.io.Serializable;
public class Transportadora extends Entregador implements EntregadorCobrador,Serializable
{
    private int nif; 
    private double precokm;
    public static double velMedia=50;
    
     public Transportadora()
    {
        super();
        this.nif=0;
        this.precokm=0;
    }
    public Transportadora(String codEmpresa,String nome,GPS gps,int nif,int raio,double precokm)
    {
        super(codEmpresa,nome,gps,raio);
        this.nif=nif;
        this.precokm=precokm;
    }
    public Transportadora(String codEmpresa,String nome,double x,double y,int nif,double raio,double precokm){
        super(codEmpresa,nome,x,y,raio);
        this.nif=nif;
        this.precokm=precokm;
    }
    public Transportadora(Transportadora transportadora){
        super(transportadora);
        this.nif=transportadora.getNif();
        this.precokm=transportadora.getPrecokm();
    }
    //t26,VeryFast,-97.98,80.00,400000,120.0,1.5
    public Transportadora(String transportadora) throws NullPointerException,NumberFormatException{
     var array = transportadora.split(",");
     //this = new Transportadora(array[0],array[1],Double.parseDouble(array[2]),Double.parseDouble(array[3]),Double.parseDouble(array[5]));
     this.setCodigo(array[0]);
     this.setNome(array[1]);
     this.setGps(Double.parseDouble(array[2]),Double.parseDouble(array[3]));
     this.nif=Integer.parseInt(array[4]);
     this.setRaio(Double.parseDouble(array[5]));
     this.precokm=Double.parseDouble(array[6]);
     this.setKmAndados(0);
     this.setClassificacao(0);
     this.setTotalVotos(0);
     this.aceitaMedicamentos(false);
    }
    public int getNif(){return this.nif;}
    public double getVelMedia(){return this.velMedia;}
    public double getPrecokm(){return this.precokm;}
    public void setNif(int nif){this.nif=nif;}
    public void setPrecokm(double precokm){this.precokm = precokm;}
    public static void setVelMedia(double x){Transportadora.velMedia=x;}
    public Transportadora clone (){return new Transportadora(this);}
    public boolean equals(Object o){
    boolean res=super.equals(o);
    if (res){
    Transportadora a=(Transportadora) o;
    return this.nif==a.getNif() && this.precokm==a.getPrecokm();
    }
    return res;
    }
    public String toString(){
    StringBuilder sb =new StringBuilder();
    sb.append("Transportadora:").append(this.getCodigo());
    sb.append(",").append(this.getNome());
    sb.append(this.getGps());
    sb.append(",").append(this.nif);
    sb.append(",").append(this.getRaio());
    sb.append(",").append(this.precokm)
    .append(",").append(this.getKmAndados())
    .append(",").append(this.getVelMedia()).append("\n");
    return sb.toString();
    }
}
