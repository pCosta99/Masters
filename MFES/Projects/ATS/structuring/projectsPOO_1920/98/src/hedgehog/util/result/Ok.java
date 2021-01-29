package hedgehog.util.result;

import hedgehog.util.maybe.Maybe;
import hedgehog.util.panic.Panic;

import java.io.Serializable;
import java.util.function.Function;

import static hedgehog.util.maybe.Maybe.Just;
import static hedgehog.util.maybe.Maybe.Nothing;
import static hedgehog.util.nil.Nil.NIL;

public final class Ok<T, E> implements Result<T, E>, Serializable {
    private static final long serialVersionUID = -3189027568230783557L;

    private static final Result<?, ?> OK_NIL = new Ok<>(NIL);
    private final T value;

    private Ok(final T value) {
        this.value = value;
    }

    @SuppressWarnings({"unchecked", "constructorName"})
    public static <T, E> Result<T, E> Ok() {
        return (Result<T, E>) OK_NIL;
    }

    @SuppressWarnings("constructorName")
    public static <T, E> Result<T, E> Ok(final T value) {
        return new Ok<>(value);
    }

    public boolean is_ok() {
        return true;
    }

    public boolean is_err() {
        return false;
    }

    public boolean contains(final T x) {
        return this.value.equals(x);
    }

    public boolean contains_err(final E _e) {
        return false;
    }

    public Maybe<T> ok() {
        return Just(this.value);
    }

    public Maybe<E> err() {
        return Nothing();
    }

    public <U> Result<U, E> map(final Function<? super T, ? extends U> op) {
        return Ok(op.apply(this.value));
    }

    public <U> U map_or_else(
        final Function<? super E, ? extends U> _fallback,
        final Function<? super T, ? extends U> map
    ) {
        return map.apply(this.value);
    }

    public <F> Result<T, F> map_err(
        final Function<? super E, ? extends F> _op
    ) {
        return Ok(this.value);
    }

    public <U> Result<U, E> and(final Result<U, E> res) {
        return res;
    }

    public <U> Result<U, E> and_then(
        final Function<? super T, Result<U, E>> op
    ) {
        return op.apply(this.value);
    }

    public Result<T, E> and_then_do(final Runnable op) {
        op.run();
        return this;
    }

    public <F> Result<T, F> or(final Result<T, F> _res) {
        return Ok(this.value);
    }

    public <F> Result<T, F> or_else(
        final Function<? super E, Result<T, F>> _op
    ) {
        return Ok(this.value);
    }

    public Result<T, E> or_else_do(final Runnable op) {
        return this;
    }

    public T unwrap() {
        return this.value;
    }

    public E unwrap_err() {
        throw new Panic(
            "called `Result.unwrap_err()` on an `Ok` value: " + this.value
        );
    }

    public T unwrap_or(final T _optb) {
        return this.value;
    }

    public T unwrap_or_else(final Function<? super E, ? extends T> _op) {
        return this.value;
    }

    public T expect(final String _msg) {
        return this.value;
    }

    public E expect_err(final String msg) {
        throw new Panic(msg);
    }
}
