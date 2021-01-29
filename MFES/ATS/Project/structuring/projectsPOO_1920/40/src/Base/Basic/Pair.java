package Base.Basic;

import java.util.Objects;

public class Pair<D, E> {
    private D first;
    private E second;

    public Pair() {
    }

    public Pair(D first, E second) {
        this.first = first;
        this.second = second;
    }

    public D getFirst() {
        return this.first;
    }

    public void setFirst(D first) {
        this.first = first;
    }

    public E getSecond() {
        return this.second;
    }

    public void setSecond(E second) {
        this.second = second;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Pair)) {
            return false;
        }

        Pair<D,E> pair = (Pair<D,E>) o;
        return Objects.equals(first, pair.first) && Objects.equals(second, pair.second);
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second);
    }

    @Override
    public String toString() {
        return "{" +
            " first='" + getFirst() + "'" +
            ", second='" + getSecond() + "'" +
            "}";
    }
    
}