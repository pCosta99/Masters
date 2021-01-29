import ClassesAux.GPS;
import java.io.Serializable;
public abstract class Entregador implements Serializable
{
    private String codigo;
    private String nome;
    private GPS gps;
    private double raio;
    private double classificacao;
    private int totalVotos;
    private boolean entregaEncMedicas;
    private double kmAndados;

     public Entregador()
    {
        this.codigo="n/a";
        this.nome="n/a";
        this.gps=new GPS();
        this.raio=0;
        this.classificacao=0;
        this.totalVotos=0;
        this.entregaEncMedicas=false;
        this.kmAndados=0;
    }
    public Entregador(String codigo,String nome,GPS gps,double raio)
    {
        this.codigo=codigo;
        this.nome=nome;
        this.gps=gps.clone();
        this.raio=raio;
        this.classificacao=0;
        this.totalVotos=0;
        this.entregaEncMedicas=false;
        this.kmAndados=0;
    }
    public Entregador(String codigo,String nome,double x,double y,double raio){
        this.codigo=codigo;
        this.nome=nome;
        this.gps=new GPS(x,y);
        this.raio=raio;
        this.classificacao=0;
        this.totalVotos=0;
        this.entregaEncMedicas=false;
        this.kmAndados=0;
    }
    public Entregador(Entregador ent){
        this.codigo=ent.getCodigo();
        this.nome=ent.getNome();
        this.gps=ent.getGps();
        this.raio=ent.getRaio();
        this.classificacao=ent.getClassificacao();
        this.totalVotos=ent.getTotalVotos();
        this.entregaEncMedicas=ent.aceitoTransporteMedicamentos();
        this.kmAndados=ent.getKmAndados();
    }
    //v69,Pedro Miguel ,-97.98,80.00
    public double getClassificacao(){return this.classificacao;}
    public double getKmAndados(){return this.kmAndados;}
    public int getTotalVotos(){return this.totalVotos;}
    public boolean aceitoTransporteMedicamentos(){return this.entregaEncMedicas;}
    public String getCodigo(){return this.codigo;}
    public String getNome(){return this.nome;}
    public GPS getGps(){return this.gps.clone();}
    public double getRaio(){return this.raio;}
    public void setClassificacao(double c){this.classificacao=c;}
    public void setTotalVotos(int v){this.totalVotos=v;}
    public void aceitaMedicamentos(boolean b){this.entregaEncMedicas=b;}
    public void setCodigo(String cod){this.codigo=cod;}
    public void setNome(String nome){this.nome=nome;}
    public void setGps(GPS gps){this.gps=gps.clone();}
    public void setGps(double x,double y){this.gps=new GPS(x,y);}
    public void setRaio(double raio){this.raio=raio;}
    public void setKmAndados(double x){this.kmAndados=x;}
    
    public void addKmAndados(double km){this.kmAndados+=km;}
    
    public abstract Entregador clone ();
    public abstract String toString();
    
    public void classifica (double voto){
    this.classificacao=(this.classificacao * this.totalVotos + voto) /(this.totalVotos+1);
    this.totalVotos=this.totalVotos+1;
    }
    
    public boolean equals(Object o){
    if (this==o) return true;
    if ((o == null) || (this.getClass() != o.getClass()))
    return false;
    Entregador p = (Entregador) o;
    return (p.getCodigo().equals(this.codigo) && 
    p.getNome().equals(this.nome) &&
    p.getGps().equals(this.gps) &&
    p.getRaio()==this.raio
     );
    }
}
