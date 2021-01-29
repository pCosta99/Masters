import ClassesAux.GPS;
import java.io.Serializable;
public class Utilizador implements Serializable
{
    private String codUtilizador;
    private String nome;
    private GPS gps;

     public Utilizador()
    {
        this.codUtilizador="n/a";
        this.nome="n/a";
        this.gps=new GPS();
    }
    public Utilizador(String codUtilizador,String nome,GPS gps)
    {
        this.codUtilizador=codUtilizador;
        this.nome=nome;
        this.gps=gps.clone();
    }
    public Utilizador(String codUtilizador,String nome,double x,double y){
        this.codUtilizador=codUtilizador;
        this.nome=nome;
        this.gps=new GPS (x,y);
    }
    public Utilizador(Utilizador utilizador){
        this.codUtilizador=utilizador.getCodUtilizador();
        this.nome=utilizador.getNome();
        this.gps=utilizador.getGps();
    }
    //u48,Francisco Manel,-97.98,80.00
    public Utilizador(String utilizador) throws NullPointerException,NumberFormatException{
     var array = utilizador.split(",");
     this.codUtilizador=array[0];
     this.nome=array[1];
     this.gps=new GPS(Double.parseDouble(array[2]),Double.parseDouble(array[3]));
    }
    public String getCodUtilizador(){return this.codUtilizador;}
    public String getNome(){return this.nome;}
    public GPS getGps(){return this.gps.clone();}
    public void setCodUtilizador(String utilizador){this.codUtilizador=utilizador;}
    public void setNome(String nome){this.nome=nome;}
    public void setGps(GPS gps){this.gps=gps.clone();}
    public Utilizador clone (){return new Utilizador(this);}
    public boolean equals(Object o){
    if (this==o) return true;
    if ((o == null) || (this.getClass() != o.getClass()))
    return false;
    Utilizador p = (Utilizador) o;
    return (p.getCodUtilizador().equals(this.codUtilizador) && 
    p.getNome().equals(this.nome) &&
    p.getGps().equals(this.gps)
     );
    }
    public String toString(){
    StringBuilder sb =new StringBuilder();
    sb.append("Utilizador:").append(this.codUtilizador);
    sb.append(",").append(this.nome);
    sb.append(this.gps).append("\n");
    return sb.toString();
    }
}
