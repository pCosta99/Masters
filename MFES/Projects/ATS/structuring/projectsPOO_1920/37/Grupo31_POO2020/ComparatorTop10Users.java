import java.util.Comparator;
import java.util.Map;
import java.io.*;
public class ComparatorTop10Users implements Comparator<Map.Entry<String, Integer>>,Serializable {
    public int compare(Map.Entry<String, Integer> o1, Map.Entry<String, Integer> o2) {
        return o2.getValue() - o1.getValue();
    }
}