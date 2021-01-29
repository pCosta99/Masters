import java.io.Serializable;
public class EncomendaNormal extends Encomenda implements Serializable
{
    public EncomendaNormal()
    {
        super();
    }
    public EncomendaNormal(EncomendaNormal v){super(v);}
    public EncomendaNormal (String a) throws NullPointerException,NumberFormatException{super(a);} 
    public EncomendaNormal clone()
    {
        return new EncomendaNormal(this);
    }
    public String toString(){
     StringBuilder sb=new StringBuilder();
     sb.append("Encomenda:").append(this.getCodEncomenda());
     sb.append(",").append(this.getCodUtilizador());
     sb.append(",").append(this.getCodLoja());
     sb.append(",").append(this.getPeso());
     //sb.append(this.getLista().toString());
     for(LinhaEncomenda a:this.getLista()) sb.append(a.toString());
     sb.append("\n");
     return sb.toString();
    }
}
