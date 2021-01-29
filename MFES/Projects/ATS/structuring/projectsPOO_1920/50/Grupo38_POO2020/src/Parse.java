import java.io.IOException;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Parse implements Serializable {

    /** Método que lê do ficheiro e atribui a informação das linhas ao catálogo respectivo
     *
     * @param cu Catálogo utilizadores
     * @param cl catálogo loja
     * @param cv catálogo voluntário
     * @param ct catálogo transportadora
     * @param ce catálogo encomenda
     * @param ca catálogo aceites
     */
    public static void parse (CatUtilizadores cu, CatLojas cl, CatVoluntarios cv, CatTransportadoras ct,CatEncomenda ce,Aceite ca){
        List<String> linhas = lerFicheiro("files/Logs.txt");
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]);
                    cu.adicionaUser(u);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    cl.adicionaLoja(l);
                    break;
                case "Voluntario":
                    Voluntario v = parseVolunt(linhaPartida[1]);
                    cv.adicionaVolunt(v);
                    break;
                case "Transportadora":
                    Transportadoras t = parseTrans(linhaPartida[1]);
                    ct.adicionaTrans(t);
                    break;
                case "Encomenda":
                    Encomenda e = parseEnc(linhaPartida[1]);
                    ce.adicionaEnc(e);
                    break;
                case "Aceite":
                    ca.adicionaAceite(linhaPartida[1]);
                    break;
                default:
                    break;
            }

        }
    }

    /** Método que divide o input em campos e cria um novo utilizador a partir desses campos retornando o mesmo
     *
     * @param input Input
     */
    public static Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        return new Utilizador(campos[0],campos[1],new Coordenadas (Double.parseDouble(campos[2]),Double.parseDouble(campos[3])),"pass",campos[0]);
    }

    /** Método que divide o input em campos e cria uma nova loja a partir desses campos retornando a mesma
     *
     * @param input Input
     */
    public static Loja parseLoja(String input){
        String[] campos = input.split(",");
        return new Loja (campos[0],campos[1],new Coordenadas (Double.parseDouble(campos[2]),Double.parseDouble(campos[3])),-1,campos[0],"pass");
    }

    /** Método que divide o input em campos e cria um novo voluntário a partir desses campos retornando o mesmo
     *
     * @param input Input
     */
    public static Voluntario parseVolunt(String input){
        String[] campos = input.split(",");
        return new Voluntario (campos[0],campos[1],Double.parseDouble(campos[4]),new Coordenadas (Double.parseDouble(campos[2]),Double.parseDouble(campos[3])),-1,false,60,campos[0],"pass",1);
    }

    /** Método que divide o input em campos e cria uma nova transportadora a partir desses campos retornando a mesma
     *
     * @param input Input
     */
    public static Transportadoras parseTrans(String input){
        String[] campos = input.split(",");
        return new Transportadoras (campos[0],campos[1],new Coordenadas (Double.parseDouble(campos[2]),Double.parseDouble(campos[3])),Double.parseDouble(campos[5]),Double.parseDouble(campos[6]),Double.parseDouble(campos[3]),-1,false,100,campos[0],"pass",0,5);
    }

    /** Método que divide o input em campos e cria uma nova encomenda a partir desses campos retornando a mesma
     *
     * @param input Input
     */
    public static Encomenda parseEnc(String input){
        int i;
        String[] campos = input.split(",");
        Encomenda a = new Encomenda (campos[0],campos[1],campos[2],Double.parseDouble(campos[3]));
        for(i=4;i<campos.length;i=i+4){
            a.adicionaLEnco(new LinhaEncomenda(campos[i],campos[i+1],Double.parseDouble(campos[i+2]),Double.parseDouble(campos[i+3])));
        }
        return a;
    }

    /** Método que retorna uma Lista com as linhas do ficheiro
     *
     * @param nomeFich Nome Ficheiro
     */
    public static List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch (IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }


}
