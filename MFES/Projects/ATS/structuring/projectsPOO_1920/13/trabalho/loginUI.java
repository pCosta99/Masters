
import java.util.*;
public class loginUI
{
    public loginUI(String tipo){
        records r = new records("log.txt");
        List<String> lista = new ArrayList<>();
        lista.add(tipo);
        r.allocar(lista);
        char t=tipo.charAt(0);
        if(t=='u'){
            new userUI(tipo,r);
        }
        if(t=='l'){
            new lojaUI(tipo,r);
        }
        if(t=='v'){
            new volUI(tipo,r);
        }
        if(t=='t'){
            new transUI(tipo,r);
        }
    }
}
