package hedgehog.util.result;

import hedgehog.util.maybe.Maybe;

import java.util.function.Function;

public interface Result<T, E> {
    boolean is_ok();

    boolean is_err();

    boolean contains(T x);

    boolean contains_err(E x);

    Maybe<T> ok();

    Maybe<E> err();

    <U> Result<U, E> map(Function<? super T, ? extends U> f);

    <U> U map_or_else(
        Function<? super E, ? extends U> fallback,
        Function<? super T, ? extends U> map
    );

    <F> Result<T, F> map_err(Function<? super E, ? extends F> op);

    <U> Result<U, E> and(Result<U, E> res);

    <U> Result<U, E> and_then(Function<? super T, Result<U, E>> op);

    Result<T, E> and_then_do(Runnable op);

    <F> Result<T, F> or(Result<T, F> res);

    <F> Result<T, F> or_else(Function<? super E, Result<T, F>> op);

    Result<T, E> or_else_do(Runnable op);

    T unwrap();

    E unwrap_err();

    T unwrap_or(T optb);

    T unwrap_or_else(Function<? super E, ? extends T> op);

    T expect(String msg);

    E expect_err(String msg);
}
