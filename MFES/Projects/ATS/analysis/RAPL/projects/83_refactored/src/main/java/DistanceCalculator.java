import java.util.*;
import java.lang.*;
import java.io.*;

class DistanceCalculator
{
    /**
     * Método que calcula a distãncia entre 2 pontos
     */
    public static double distance(double lat1, double lat2, double lon1, double lon2)
    {

        return Math.sqrt((lon2 - lon1) * (lon2 - lon1) + (lat2 - lat1) * (lat2 - lat1));
    }
}
