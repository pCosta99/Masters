import java.util.Comparator;

public class DateTimeComparatorEE implements Comparator<EncomendaEfetuada>
{
    public int compare(EncomendaEfetuada e1,EncomendaEfetuada e2){
        int i = e1.getTempo().compareTo(e2.getTempo());
        if (i==0) return 1;
        return i;
    }
  
}
