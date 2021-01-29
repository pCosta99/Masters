import Exceptions.*;
import java.io.Serializable;

public class Condicoes implements Serializable
{
    private int metreologia;           //[1,2,3,4,5][pessimo,desfavoravel,normal,favoravel,otimo]
    private int transito;              //[1,2,3,4,5][pessimo,desfavoravel,normal,favoravel,otimo]
    private boolean feriado;
    private double fator;              //max:4.5(m=1,t=1,f=true) min:0.25(m=5,t=5,f=false)
    
    public Condicoes()
    {
        this.metreologia=3; //this.tempoPorFila=0.08; //+/- 5 mins
        this.transito=3;
        this.feriado=false;
        this.fator=1;
    }
    public Condicoes(Condicoes p){
        this.metreologia=p.getMetreologia();
        this.transito=p.getTransito();
        this.feriado=p.getFeriado();
        this.fator=p.getFator();
     }
    public void setMetreologia(int i) throws ValorInvalido{
        if (i<0 || i>5) throw new ValorInvalido();
        this.metreologia=i;
    }
    public void setTransito(int i) throws ValorInvalido{
        if (i<0 || i>5) throw new ValorInvalido();
        this.transito=i;
    }
    public void setFeriado(boolean s){this.feriado=s;}
    public void setFator(double x){this.fator=x;}
    public int getMetreologia(){return this.metreologia;}
    public int getTransito (){return this.transito;}
    public boolean getFeriado(){return this.feriado;}
    public double getFator (){return this.fator;}
    public Condicoes clone(){return new Condicoes (this);}
    public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Condicoes:").append(metreologia).append(",")
    .append(transito).append(",").append(feriado).append(",")
    .append(fator).append("\n");
    return sb.toString();
    }
    public void atualizaFator(int metreologia,int transito,boolean feriado) throws ValorInvalido{
        if (metreologia<0 || metreologia>5) throw new ValorInvalido();
        if (transito<0 || transito>5) throw new ValorInvalido();
        this.metreologia=metreologia;
        this.transito=transito;
        this.feriado=feriado;
        this.fator=((-0.25*metreologia+1.75)*(-0.25*transito+1.75));
        if(feriado) this.fator*=2;
    }
    public boolean equals(Object o){
        if (this==o) return true;
        if ((o == null) || (this.getClass() != o.getClass())) return false;
        Condicoes p = (Condicoes) o;
        return (p.getMetreologia()==this.metreologia) && 
        (p.getTransito()==this.transito) &&
        (p.getFator()==this.fator) && (p.getFeriado()==this.feriado);
    }
}
