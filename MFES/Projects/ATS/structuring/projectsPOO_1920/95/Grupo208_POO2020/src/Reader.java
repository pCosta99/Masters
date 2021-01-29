import java.util.List;
import java.util.ArrayList;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;

/**
 * Classe que lê um ficheiro.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Reader {

    /**
     * Função que retorna uma List<String> e cujas String dizem respeito a cada uma das linhas do ficheiro.
     * @param nomeFich, Nome do ficheiro a ler.
     * @return List.
     */
    public static List<String> readFile(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }
}
