import java.io.Serializable;
import Interfaces.EntregadorPontos;
import ClassesAux.GPS;
public class Voluntario extends Entregador implements Serializable,EntregadorPontos
{
    double pontos;
    public static double pontosPorKm= 3.0;
     public Voluntario()
    {   super();
        this.pontos=0;
    }
    public Voluntario(String codVoluntario,String nome,GPS gps,double raio)
    {
        super(codVoluntario,nome,gps,raio);
        this.pontos=0;
    }
    public Voluntario(String codVoluntario,String nome,double x,double y,double raio){
        super(codVoluntario,nome,x,y,raio);
        this.pontos=0;
    }
    public Voluntario(Voluntario voluntario){
        super(voluntario);
        pontos=voluntario.getPontos();
    }
    //v73,Rui Pedro Gomes Coelho,-45.424522,-79.517136,15.0
     public Voluntario(String voluntario) throws NullPointerException,NumberFormatException{
     var array = voluntario.split(",");
     this.setCodigo(array[0]);
     this.setNome(array[1]);
     this.setGps(Double.parseDouble(array[2]),Double.parseDouble(array[3]));
     this.setRaio(Double.parseDouble(array[4]));
     this.setKmAndados(0);
     this.setClassificacao(0);
     this.setTotalVotos(0);
     this.aceitaMedicamentos(false);
     this.pontos=0;
    }
    public double getPontos(){return this.pontos;}
    public void setPontos(double x){this.pontos=x;}
    public void somaPontos(double kmAndados){this.pontos+=(kmAndados * pontosPorKm);}
    public void retiraPontos(double pontos){this.pontos-=pontos;}
    public static double getPontosPorKm(){return pontosPorKm;}
    public static void setPontosPorKm(double x){Voluntario.pontosPorKm=x;}
    public Voluntario clone (){return new Voluntario(this);}
    public boolean equals(Object o){return super.equals(o);}
    public String toString(){
    StringBuilder sb =new StringBuilder();
    sb.append("Voluntario:").append(this.getCodigo());
    sb.append(",").append(this.getNome());
    sb.append(this.getGps()).append(",").append(this.getRaio()).append(",")
    .append(this.pontos).append("\n");
    return sb.toString();
    }
}
