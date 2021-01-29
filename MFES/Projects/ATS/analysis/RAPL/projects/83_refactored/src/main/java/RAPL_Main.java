import java.nio.file.Files;
import java.nio.file.Paths;

public class RAPL_Main {
    public static void main(String[] args) {
        if(!Files.exists(Paths.get("logs.txt"))) System.err.println("Couldn't find log file!");
        Parse parser = new Parse();
        parser.parse();
        BDGeral base = new BDGeral(parser.getBaseGeral());
        base.top10Encomendas();
        base.top10KmsPercorridos();
    }
}
