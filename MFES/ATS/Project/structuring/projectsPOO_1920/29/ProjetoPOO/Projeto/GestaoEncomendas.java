import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.io.Serializable;
import java.lang.NullPointerException;

public class GestaoEncomendas implements Serializable
{   
    public double custoT(Loja l, Utilizador u, Transportadora t, Encomenda e){
        int x = 0;
        double distLoja = Math.sqrt(Math.pow(t.getGPSE().getLongitude() + l.getGPSL().getLongitude(), 2) - Math.pow(t.getGPSE().getLatitude() + l.getGPSL().getLatitude(), 2));
        double distLojaCasa = Math.sqrt(Math.pow(l.getGPSL().getLongitude() + u.getGPS().getLongitude(), 2) - Math.pow(l.getGPSL().getLatitude() + u.getGPS().getLatitude(), 2));
        for (LinhaEncomenda a: e.getLinha()) {
            x += a.getValorUnitario();
        }    
        double custo = x + e.getPeso() + (distLoja * t.getPrecoKm()) + (distLojaCasa * t.getPrecoKm());
        return custo;
    }
    
    public double custoV(Loja l, Utilizador u, Voluntario v, Encomenda e) throws NullPointerException{
        int x = 0;
        double distLoja = Math.sqrt(Math.pow(v.getGPSV().getLongitude() + l.getGPSL().getLongitude(), 2) - Math.pow(v.getGPSV().getLatitude() + l.getGPSL().getLatitude(), 2));
        double distLojaCasa = Math.sqrt(Math.pow(l.getGPSL().getLongitude() + u.getGPS().getLongitude(), 2) - Math.pow(l.getGPSL().getLatitude() + u.getGPS().getLatitude(), 2));
        for (LinhaEncomenda a: e.getLinha()) {
            x += a.getValorUnitario();
        }    
        double custo = x + e.getPeso();
        return custo;
    }
    
    
   public double inRangeLtoUkm(Loja l,Utilizador u){ 
    return Math.sqrt(Math.pow(l.getGPSL().getLongitude() + u.getGPS().getLongitude(), 2) - Math.pow(l.getGPSL().getLatitude() + u.getGPS().getLatitude(), 2));
    } 
   
   public double kmtotal(Transportadora t , Loja l , Utilizador u){ 
    return t.inRangeTtoLkm(l)+inRangeLtoUkm(l,u);
    }
}
