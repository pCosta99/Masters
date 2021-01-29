package hedgehog.util.point;

import java.io.Serializable;

public final class Point implements PointView, Serializable {
    private static final long serialVersionUID = 1473435728097747882L;

    public double x;
    public double y;

    private Point(final double x, final double y) {
        this.x = x;
        this.y = y;
    }

    public static Point at(final double x, final double y) {
        return new Point(x, y);
    }

    public double x() {
        return this.x;
    }

    public double y() {
        return this.y;
    }

    public Point move_to(final PointView other) {
        this.x = other.x();
        this.y = other.y();
        return this;
    }
}
