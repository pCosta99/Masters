package Model;

import java.util.Comparator;
import java.util.AbstractMap.SimpleEntry;

public class CompareByName implements Comparator<SimpleEntry<String,Double>> {
    public int compare(SimpleEntry<String,Double> s1, SimpleEntry<String, Double> s2) {
        return (int) (s2.getKey().compareTo(s1.getKey()));
    }
}