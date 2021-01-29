package model;

import java.util.Comparator;

public class ComparatorNrEncomendas implements Comparator<User> {

    public int compare(User c1, User c2){
        int a = c1.numeroEncomendas();
        int b = c2.numeroEncomendas();

        if(a == b ) return 0;
        if(a > b) return -1;
        if(a < b) return 1;
        return 0;
    }
}
