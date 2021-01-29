

import java.util.Comparator;

public class ComparatorTop10Utilizadores implements Comparator<Utilizador> {

    public int compare(Utilizador u1, Utilizador u2){
     if (u2.getUser_historico().size() > u1.getUser_historico().size()){
         return 1;
     }else{
         return -1;
     }
    }
}
