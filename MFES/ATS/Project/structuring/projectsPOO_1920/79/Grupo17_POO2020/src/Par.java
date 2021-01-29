import java.io.Serializable;


public class Par implements Serializable
{
    private String p1;
    private String p2;

    Par(){
        this.p1 = new String();
        this.p2 = new String();
    }

    Par(String p1,String p2){
        this.p1 = p1;
        this.p2 = p2;
    }

    Par(Par p){
        setP1(p.getP1());
        setP2(p.getP2());
    }

    public String getP1(){return this.p1;}

    public String getP2(){return this.p2;}

    public void setP1(String novo){this.p1 = novo;}

    public void setP2(String novo){this.p2 = novo;}

    public Par clone(){return new Par(this);}

    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass()!=this.getClass())return false;
        Par p = (Par) o;
        return this.p1.equals(p.getP1()) && this.p2.equals(p.getP2());
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Loja: ").append(this.p1).append(" , ").append("Codigo: ").append(this.p2);
        return sb.toString();
    }
}