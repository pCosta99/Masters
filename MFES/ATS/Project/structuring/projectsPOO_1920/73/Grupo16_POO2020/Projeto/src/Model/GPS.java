package Model;

import java.io.Serializable;

public class GPS implements Serializable {
    double x;
    double y;

    public GPS() {
        this.x = 0;
        this.y = 0;
    }

    public GPS(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return this.x;
    }

    public void setX(int x) {
        this.x = x;
    }

    public double getY() {
        return this.y;
    }

    public void setY(int y) {
        this.y = y;
    }

    @Override
    public String toString() {
        return "GPS{" +
                "x=" + x +
                ", y=" + y +
                '}';
    }
}
