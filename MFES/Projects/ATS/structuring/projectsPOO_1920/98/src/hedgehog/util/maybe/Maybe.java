package hedgehog.util.maybe;

import hedgehog.util.panic.Panic;
import hedgehog.util.result.Result;

import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.io.Serializable;
import java.util.Optional;

import static hedgehog.util.result.Err.Err;
import static hedgehog.util.result.Ok.Ok;

public final class Maybe<T> implements Serializable {
    private static final long serialVersionUID = 5322954702151224711L;

    private static final Maybe<?> NOTHING = new Maybe<>();
    private T value;

    private Maybe() {
        this.value = null;
    }

    private Maybe(final T value) {
        this.value = value;
    }

    public static <T> Maybe<T> Just(final T value) {
        return new Maybe<>(value);
    }

    @SuppressWarnings("unchecked")
    public static <T> Maybe<T> Nothing() {
        return (Maybe<T>) NOTHING;
    }

    public static <T> Maybe<T> from_optional(final Optional<T> opt) {
        return opt.isEmpty() ? Nothing() : Just(opt.get());
    }

    public static <T> Maybe<T> from_nullable(final T maybe) {
        return maybe == null ? Nothing() : Just(maybe);
    }

    public boolean is_just() {
        return this.value != null;
    }

    public boolean is_nothing() {
        return this.value == null;
    }

    public boolean contains(final T x) {
        return this.is_just() && this.value.equals(x);
    }

    public T expect(final String msg) {
        if (this.is_just()) {
            return this.value;
        } else {
            throw new Panic(msg);
        }
    }

    public void expect_none(final String msg) {
        if (this.is_just()) {
            throw new Panic(msg);
        }
    }

    public T unwrap() {
        if (this.is_just()) {
            return this.value;
        } else {
            throw new Panic("called `Maybe.unwrap()` on a `Nothing` value");
        }
    }

    public void unwrap_none() {
        if (this.is_just()) {
            throw new Panic(
                "called `Maybe.unwrap_none()` on a `Just` value: " + this.value
            );
        }
    }

    public T unwrap_or(final T def) {
        return this.is_just() ? this.value : def;
    }

    public T unwrap_or_else(final Supplier<? extends T> f) {
        return this.is_just() ? this.value : f.get();
    }

    public <U> Maybe<U> map(final Function<? super T, ? extends U> f) {
        return this.is_just() ? Just(f.apply(this.value)) : Nothing();
    }

    public <U> U map_or(final U def, final Function<? super T, ? extends U> f) {
        return this.is_just() ? f.apply(this.value) : def;
    }

    public <U> U map_or_else(
        final Supplier<? extends U> def,
        final Function<? super T, ? extends U> f
    ) {
        return this.is_just() ? f.apply(this.value) : def.get();
    }

    public <E> Result<T, E> ok_or(final E err) {
        return this.is_just() ? Ok(this.value) : Err(err);
    }

    public <E> Result<T, E> ok_or_else(final Supplier<? extends E> err) {
        return this.is_just() ? Ok(this.value) : Err(err.get());
    }

    public <U> Maybe<U> and(final Maybe<U> optb) {
        return this.is_just() ? optb : Nothing();
    }

    public <U> Maybe<U> and_then(final Function<? super T, Maybe<U>> f) {
        return this.is_just() ? f.apply(this.value) : Nothing();
    }

    public Maybe<T> and_then_do(final Runnable op) {
        if (this.is_just()) {
            op.run();
        }
        return this;
    }

    public Maybe<T> or(final Maybe<T> optb) {
        return this.is_just() ? this : optb;
    }

    public Maybe<T> or_else(final Supplier<Maybe<T>> f) {
        return this.is_just() ? this : f.get();
    }

    public Maybe<T> or_else_do(final Runnable op) {
        if (this.value == null) {
            op.run();
        }
        return this;
    }

    public Maybe<T> filter(final Predicate<? super T> predicate) {
        return this.is_just() && predicate.test(this.value)
            ? this
            : Nothing();
    }

    public Maybe<T> xor(final Maybe<T> optb) {
        final var is_just_b = optb.is_just();

        if (this.is_just()) {
            return is_just_b ? Nothing() : Just(this.value);
        } else {
            return is_just_b ? Just(optb.value) : Nothing();
        }
    }

    public T get_or_insert(final T v) {
        if (this.is_nothing()) {
            this.value = v;
        }
        return this.value;
    }

    public T get_or_insert_with(final Supplier<? extends T> f) {
        if (this.is_nothing()) {
            this.value = f.get();
        }
        return this.value;
    }

    public Maybe<T> replace(final T value) {
        final var old_value = this.value;
        this.value = value;
        return old_value != null ? Just(old_value) : Nothing();
    }
}
