package hedgehog.model.emissary;

import hedgehog.model.account.Account;
import hedgehog.model.account.AccountView;
import hedgehog.model.order.Order;
import hedgehog.model.weather.WeatherCaster.Weather;
import hedgehog.util.maybe.Maybe;
import hedgehog.util.nil.Nil;
import hedgehog.util.point.*;
import hedgehog.util.result.Result;

import static hedgehog.util.maybe.Maybe.Nothing;
import static hedgehog.util.result.Err.Err;
import static hedgehog.util.result.Ok.Ok;

import java.io.Serializable;
import java.util.HashMap;

public final class Firm implements Serializable {
    private static final long serialVersionUID = -5487371351557598776L;

    public static final String DEFAULT_EMAIL_DOMAIN = "@firm.trazaqui.com";
    public static final String DEFAULT_PASSWORD_SUFFIX = "tr@zAqu1";
    public static final double DEFAULT_BASE_DELIVERY_SPEED = 50;  // km/h
    public int code;
    public boolean is_medically_certified;
    public int vat;  // value-added tax (PT: nif)
    private final Account account;
    private final Point location;
    private double base_delivery_speed;  // km/h, can be affected by weather
    private double delivery_range;  // radius in km
    private double distance_fare; // eur/km
    private double mileage;  // km
    private final Ratings ratings;
    private HashMap<Integer, Order> current_orders;  // K = order.code
    public Maybe<Double> delivery_time;  // hours

    public enum Error {
        NON_POSITIVE_BASE_DELIVERY_SPEED,
        NON_POSITIVE_DELIVERY_RANGE,
        NON_POSITIVE_DISTANCE_FARE,
    }

    public static double delay_factor_of(final Weather weather) {
        switch (weather) {
        case SUNNY:
            return 1.0;
        case RAINY:
            return 1.4;
        case SNOWY:
            return 2.1;
        default:
            throw new RuntimeException("Unexpected weather conditions");
        }
    }

    private Firm(
        final int code,
        final boolean is_medically_certified,
        final Account account,
        final Point location,
        final int vat,
        final double base_delivery_speed,
        final double delivery_range,
        final double distance_fare,
        final double mileage,
        final Ratings ratings,
        final HashMap<Integer, Order> current_orders,
        final Maybe<Double> delivery_time
    ) {
        this.code = code;
        this.account = account;
        this.location = location;
        this.vat = vat;
        this.base_delivery_speed = base_delivery_speed;
        this.delivery_range = delivery_range;
        this.distance_fare = distance_fare;
        this.mileage = mileage;
        this.is_medically_certified = is_medically_certified;
        this.ratings = ratings;
        this.current_orders = current_orders;
        this.delivery_time = delivery_time;
    }

    public static Result<Firm, Error> of(
        final int code,
        final Account account,
        final Point location,
        final int vat,
        final double base_delivery_speed,
        final double delivery_range,
        final double distance_fare
    ) {
        if (base_delivery_speed < 0.) {
            return Err(Error.NON_POSITIVE_BASE_DELIVERY_SPEED);
        }
        if (delivery_range < 0.) {
            return Err(Error.NON_POSITIVE_DELIVERY_RANGE);
        }
        if (distance_fare < 0.) {
            return Err(Error.NON_POSITIVE_DISTANCE_FARE);
        }
        return Ok(new Firm(
            code,
            false,
            account,
            location,
            vat,
            base_delivery_speed,
            delivery_range,
            distance_fare,
            0.,
            Ratings.empty(),
            new HashMap<>(),
            Nothing()
        ));
    }

    public AccountView account() {
        return this.account;
    }

    public Account account_mut() {
        return this.account;
    }

    public PointView location() {
        return this.location;
    }

    public Point location_mut() {
        return this.location;
    }

    public double base_delivery_speed() {
        return this.base_delivery_speed;
    }

    public double delivery_range() {
        return this.delivery_range;
    }

    public double distance_fare() {
        return this.distance_fare;
    }

    public double mileage() {
        return this.mileage;
    }

    public Maybe<Double> ratings() {
        return this.ratings.get();
    }

    public long ratings_count() {
        return this.ratings.count();
    }

    public Result<Nil, Ratings.Error> rate(final double rating) {
        return this.ratings.rate(rating);
    }

    public void reset_ratings() {
        this.ratings.reset();
    }

    // WARNING: implementation may change
    public boolean is_available() {
        return this.current_orders.isEmpty();
    }
}
