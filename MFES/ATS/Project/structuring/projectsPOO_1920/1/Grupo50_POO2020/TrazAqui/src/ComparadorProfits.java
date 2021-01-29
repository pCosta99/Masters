import java.util.Comparator;

public class ComparadorProfits implements Comparator<Integer> {
    public ComparadorProfits() {
    }

    @Override



    public int compare(Integer i1, Integer i2) {
        float r = i1 - i2;
        if (r < 0) {
            return -1;
        }
        else if (r > 0){
            return 1;
        }
        return 0;
    }
}
