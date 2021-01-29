package Model;

public class Auxiliar {

    static public boolean validUserName(String userName) {
        Character letter = userName.charAt(0);
        return ((letter == 'u' || letter == 'v' || letter == 't' || letter == 'l'));
    }

    static public int voluntarioOuTransportadora(String cod) {
        Character letter = cod.charAt(0);
        if (letter == 'v') {
            return 0; // Caso Tenha Sido Um Voluntário.
        }
        else {
            return 1; // Caso Tenha Sido Uma Transportadora.
        }
    }

    static public double calculadoraDistancia (double x1, double y1, double x2, double y2) {
        double rad = Math.PI/180;
        double R = 6378.137;
        double xFinal = rad * (x2 - x1);
        double yFinal = rad * (y2 - y1);
        double a = Math.sin(xFinal/2) * Math.sin(xFinal/2) + Math.cos(rad * x1) * Math.cos(rad * x2) * Math.sin(yFinal/2) * Math.sin(yFinal/2);
        double b = 2 * Math.atan2(Math.sqrt(a),Math.sqrt(1-a));
        return (R * b);
    }
}