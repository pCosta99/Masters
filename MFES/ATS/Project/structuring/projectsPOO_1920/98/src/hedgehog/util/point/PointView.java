package hedgehog.util.point;

import static java.lang.Math.sqrt;

public interface PointView {
    private static double abs_dist(final double a, final double b) {
        return (a > b) ? (a - b) : (b - a);
    }

    default double distance_to(final PointView other) {
        final var x_dist = abs_dist(this.x(), other.x());
        final var y_dist = abs_dist(this.y(), other.y());
        return sqrt(x_dist * x_dist + y_dist * y_dist);
    }

    double x();

    double y();
}
