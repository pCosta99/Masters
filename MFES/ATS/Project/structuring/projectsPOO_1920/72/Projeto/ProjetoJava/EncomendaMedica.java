import java.util.ArrayList;
import java.io.Serializable;
 class EncomendaMedica extends Encomenda implements Serializable
{

    public EncomendaMedica()
    {
        super();
    }
    public EncomendaMedica(EncomendaMedica v){super(v);}
    public EncomendaMedica (String a) throws NullPointerException,NumberFormatException{super(a);}
    public EncomendaMedica clone()
    {
        return new EncomendaMedica(this);
    }
    public String toString(){
     StringBuilder sb=new StringBuilder();
     sb.append("EncomendaMedica:").append(this.getCodEncomenda());
     sb.append(",").append(this.getCodUtilizador());
     sb.append(",").append(this.getCodLoja());
     sb.append(",").append(this.getPeso());
     //sb.append(this.getLista().toString());
     for(LinhaEncomenda a:this.getLista()) sb.append(a.toString());
     sb.append("\n");
     return sb.toString();
    }
}
