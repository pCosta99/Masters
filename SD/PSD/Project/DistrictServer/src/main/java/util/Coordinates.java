package util;

import java.util.Objects;

public class Coordinates {

    private final Pair<Integer, Integer> geoCoordinates;

    public Coordinates(int fst, int snd){
        this.geoCoordinates = new Pair<>(fst, snd);
    }

    public Coordinates(Coordinates other){
        this.geoCoordinates = new Pair<>(other.getFirstCoordinate(), other.getSecondCoordinate());
    }

    public int getFirstCoordinate() {
        return this.geoCoordinates.first;
    }

    public void setFirstCoordinate(int x) {
        this.geoCoordinates.first = x;
    }

    public int getSecondCoordinate() {
        return this.geoCoordinates.second;
    }

    public void setSecondCoordinate(int y) {
        this.geoCoordinates.second = y;
    }

    public void updateFirstCoordinateBy(int value){
        this.geoCoordinates.first += value;
    }

    public void updateSecondCoordinateBy(int value){
        this.geoCoordinates.second += value;
    }

    public static Coordinates parseFrom(String coordinates){
        String [] split = coordinates.split(",");
        String fstString = split[0].replace("[", "").replace(" ", "");
        String sndString = split[1].replace("]", "").replace(" ", "");

        return new Coordinates(Integer.parseInt(fstString), Integer.parseInt(sndString));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Coordinates that = (Coordinates) o;
        return that.getFirstCoordinate() == this.getFirstCoordinate() && that.getSecondCoordinate() == this.getSecondCoordinate();
    }

    @Override
    public int hashCode() {
        return geoCoordinates.first.hashCode() * geoCoordinates.second.hashCode();
    }
}
