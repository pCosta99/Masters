package MVC.Comparators;

import MVC.Models.BaseModels.GPS;
import MVC.Models.BaseModels.User;

import java.util.Comparator;

public class DistanciaLojaComparator implements Comparator<User> {

    private GPS gpsLoja;

    public DistanciaLojaComparator(GPS g){
        this.gpsLoja = g;
    }

    public int compare(User u1, User u2){

        double dist_u1 = this.gpsLoja.distancia(u1.getGPS());
        double dist_u2 = this.gpsLoja.distancia(u2.getGPS());

        if (dist_u1 < dist_u2)
            return -1;
        if (dist_u1 > dist_u2)
            return 1;

        return u1.getCod().compareTo(u2.getCod());
    }
}
