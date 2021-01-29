package hedgehog.util.result;

import hedgehog.util.maybe.Maybe;
import hedgehog.util.panic.Panic;

import java.io.Serializable;
import java.util.function.Function;

import static hedgehog.util.maybe.Maybe.Just;
import static hedgehog.util.maybe.Maybe.Nothing;

public final class Err<T, E> implements Result<T, E>, Serializable {
    private static final long serialVersionUID = 241116029088579577L;

    private final E error;

    private Err(final E error) {
        this.error = error;
    }

    @SuppressWarnings("constructorName")
    public static <T, E> Result<T, E> Err(final E error) {
        return new Err<>(error);
    }

    public boolean is_ok() {
        return false;
    }

    public boolean is_err() {
        return true;
    }

    public boolean contains(final T _x) {
        return false;
    }

    public boolean contains_err(final E e) {
        return this.error.equals(e);
    }

    public Maybe<T> ok() {
        return Nothing();
    }

    public Maybe<E> err() {
        return Just(this.error);
    }

    public <U> Result<U, E> map(final Function<? super T, ? extends U> _op) {
        return Err(this.error);
    }

    public <U> U map_or_else(
        final Function<? super E, ? extends U> fallback,
        final Function<? super T, ? extends U> _map
    ) {
        return fallback.apply(this.error);
    }

    public <F> Result<T, F> map_err(final Function<? super E, ? extends F> op) {
        return Err(op.apply(this.error));
    }

    public <U> Result<U, E> and(final Result<U, E> _res) {
        return Err(this.error);
    }

    public <U> Result<U, E> and_then(
        final Function<? super T, Result<U, E>> _op
    ) {
        return Err(this.error);
    }

    public Result<T, E> and_then_do(final Runnable op) {
        return Err(this.error);
    }

    public <F> Result<T, F> or(final Result<T, F> res) {
        return res;
    }

    public <F> Result<T, F> or_else(
        final Function<? super E, Result<T, F>> op
    ) {
        return op.apply(this.error);
    }

    public Result<T, E> or_else_do(final Runnable op) {
        op.run();
        return this;
    }

    public T unwrap() {
        throw new Panic(
            "called `Result.unwrap()` on an `Err` value: " + this.error
        );
    }

    public E unwrap_err() {
        return this.error;
    }

    public T unwrap_or(final T optb) {
        return optb;
    }

    public T unwrap_or_else(final Function<? super E, ? extends T> op) {
        return op.apply(this.error);
    }

    public T expect(final String msg) {
        throw new Panic(msg);
    }

    public E expect_err(final String _msg) {
        return this.error;
    }
}
