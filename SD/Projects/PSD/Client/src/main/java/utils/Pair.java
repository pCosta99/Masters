package utils;

public class Pair<A,B> {
    private final A fst;
    private final B snd;

    public Pair(A a, B b){
        fst = a;
        snd = b;
    }

    public A getFst() {
        return fst;
    }

    public B getSnd() {
        return snd;
    }
}
