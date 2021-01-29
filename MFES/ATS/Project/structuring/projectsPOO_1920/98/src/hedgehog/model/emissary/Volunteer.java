package hedgehog.model.emissary;

import hedgehog.model.account.Account;
import hedgehog.model.account.AccountView;
import hedgehog.model.order.Order;
import hedgehog.model.weather.WeatherCaster.Weather;
import hedgehog.util.maybe.Maybe;
import hedgehog.util.nil.Nil;
import hedgehog.util.point.*;
import hedgehog.util.result.Result;

import java.io.Serializable;

import static hedgehog.util.maybe.Maybe.Nothing;
import static hedgehog.util.result.Err.Err;
import static hedgehog.util.result.Ok.Ok;

public final class Volunteer implements Serializable {
    private static final long serialVersionUID = -6135834108882739131L;

    public static final String DEFAULT_EMAIL_DOMAIN = "@volunteer.trazaqui.com";
    public static final String DEFAULT_PASSWORD_SUFFIX = "tr@zAqu1";
    public static final double DEFAULT_BASE_DELIVERY_SPEED = 40;
    public int code;  // not sure what this is
    public boolean is_medically_certified;
    private final Account account;
    private final Point location;
    private double base_delivery_speed;  // km/h, can be affected by weather
    private double delivery_range;  // radius in km
    private final Ratings ratings;
    private Maybe<Order> current_order;
    public Maybe<Double> delivery_time;  // hours

    public enum Error {
        NON_POSITIVE_BASE_DELIVERY_SPEED,
        NON_POSITIVE_DELIVERY_RANGE,
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

    public Volunteer(
        final int code,
        final boolean is_medically_certified,
        final Account account,
        final Point location,
        final double base_delivery_speed,
        final double delivery_range,
        final Ratings ratings,
        final Maybe<Order> current_order,
        final Maybe<Double> delivery_time
    ) {
        this.code = code;
        this.account = account;
        this.location = location;
        this.base_delivery_speed = base_delivery_speed;
        this.delivery_range = delivery_range;
        this.is_medically_certified = is_medically_certified;
        this.ratings = ratings;
        this.current_order = current_order;
        this.delivery_time = delivery_time;
    }

    public static Result<Volunteer, Error> of(
        final int code,
        final Account account,
        final Point location,
        final double base_delivery_speed,
        final double delivery_range
    ) {
        if (base_delivery_speed < 0.) {
            return Err(Error.NON_POSITIVE_BASE_DELIVERY_SPEED);
        }
        if (delivery_range < 0.) {
            return Err(Error.NON_POSITIVE_DELIVERY_RANGE);
        }

        return Ok(new Volunteer(
            code,
            false,
            account,
            location,
            base_delivery_speed,
            delivery_range,
            Ratings.empty(),
            Nothing(),
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
        return this.current_order.is_nothing();
    }
}
