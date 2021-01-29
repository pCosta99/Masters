import java.util.Comparator;

public class ComparatorET implements Comparator<EmpresaTransportadora>
{
    public int compare(EmpresaTransportadora et1,EmpresaTransportadora et2){
        if (et1.getKmsPercorridos()>et2.getKmsPercorridos()) return -1;
        if (et1.getKmsPercorridos()<et2.getKmsPercorridos()) return 1;
        return 1;
    }
  
}

