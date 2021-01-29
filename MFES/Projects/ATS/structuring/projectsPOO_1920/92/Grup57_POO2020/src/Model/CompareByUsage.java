package Model;

import java.util.Comparator;
import java.util.AbstractMap.SimpleEntry;

public class CompareByUsage implements Comparator<SimpleEntry<String,Integer>> {
    public int compare(SimpleEntry<String, Integer> s1, SimpleEntry<String, Integer> s2) {
        return s2.getValue() - s1.getValue();
    }
}