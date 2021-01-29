import java.awt.geom.Point2D;

public class TestaDistancias {
    public static void main(String[] args){
        System.out.println(testeDist());

    }

    public static double testeDist() {
        Point2D.Double loja = new Point2D.Double(39.627502,33.60112);
        Point2D.Double p2 = new Point2D.Double(-97.28862,59.067047);
        Point2D.Double p3 = new Point2D.Double(-68.78327,-50.26914);


        double dist = loja.distance(p3);
        return dist;
    }


}
