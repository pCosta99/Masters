import java.util.Comparator;
import java.io.IOException;

public class Comparatora implements Comparator<UmUser>
{
 public int compare (UmUser a1, UmUser a2) {
     try{
     int numa1 = a1.nropedidos();
     int numa2 = a2.nropedidos();
     
     if (numa1<numa2) return 1;
     if (numa2<numa1) return -1;
     if (numa1==0) return 0; 
     if (numa2==0) return 0;
     return 1;
    }
    catch (IOException e){System.out.println("erro");}
    return 1;
    }
}