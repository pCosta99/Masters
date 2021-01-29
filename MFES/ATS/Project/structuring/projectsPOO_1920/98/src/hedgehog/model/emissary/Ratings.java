package hedgehog.model.emissary;

import hedgehog.util.maybe.Maybe;
import hedgehog.util.nil.Nil;
import hedgehog.util.result.Result;

import static hedgehog.util.maybe.Maybe.Just;
import static hedgehog.util.maybe.Maybe.Nothing;
import static hedgehog.util.result.Err.Err;
import static hedgehog.util.result.Ok.Ok;

import static java.lang.Math.fma;

import java.io.Serializable;

public final class Ratings implements Serializable {
    private static final long serialVersionUID = 1491430709458111121L;

    private static final double MIN_RATING = 0.0;
    private static final double MAX_RATING = 5.0;
    private double current_rating;
    private long count;

    public enum Error {
        RATING_BELOW_MIN,
        RATING_OVER_MAX,
        REACHED_MAX_RATINGS_AMOUNT,
    }

    public static boolean is_valid_rating(final double rating) {
        return rating >= MIN_RATING && rating <= MAX_RATING;
    }

    public static Ratings empty() {
        return new Ratings();
    }

    public static Result<Ratings, Error> of(final double first_rating) {
        if (first_rating < MIN_RATING) {
            return Err(Error.RATING_BELOW_MIN);
        } else if (first_rating > MAX_RATING) {
            return Err(Error.RATING_OVER_MAX);
        } else {
            return Ok(new Ratings(first_rating));
        }
    }

    private Ratings() {
        this.current_rating = 0.0;
        this.count = 0;
    }

    private Ratings(final double first_rating) {
        this.current_rating = first_rating;
        this.count = 1;
    }

    public boolean exist() {
        return this.count != 0;
    }

    public Maybe<Double> get() {
        return this.exist() ? Just(this.current_rating) : Nothing();
    }

    public long count() {
        return this.count;
    }

    public Result<Nil, Error> rate(final double rating) {
        if (rating < MIN_RATING) {
            return Err(Error.RATING_BELOW_MIN);
        }
        if (rating > MAX_RATING) {
            return Err(Error.RATING_OVER_MAX);
        }
        if (this.count == Long.MAX_VALUE) {
            return Err(Error.REACHED_MAX_RATINGS_AMOUNT);
        }

        if (!this.exist()) {
            this.current_rating = rating;
        } else {
            this.current_rating = fma(this.current_rating, this.count, rating)
                / (this.count + 1);
        }

        ++this.count;
        return Ok();
    }

    public void reset() {
        this.count = 0;
    }
}
