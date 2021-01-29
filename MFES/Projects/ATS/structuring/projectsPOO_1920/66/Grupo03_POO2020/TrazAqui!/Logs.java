import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * Classe que trata de obter a informação dos logs
 */
public class Logs {

    /**
     * Método que faz parsing do ficheiro dos logs
     * @param r Registo
     * @throws InterruptedException Exceção
     */
    public void parse(Registo r) throws InterruptedException {
        List<String> linhas = lerFicheiro("Logs.txt");
        String[] linhaPartida;
        int u1 = 0,v1 = 0,t1 = 0, l1 = 0, e1 = 0, a1 = 0;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            TreeMap<String, RegistoTULV> m;
            switch (linhaPartida[0]) {
                case "Utilizador":
                    if (u1 != 0) {
                        Utilizador u = parseUtilizador(linhaPartida[1]);
                        m = r.getRegistos();
                        m.put(u.getCodigo(), u);
                        r.setRegistos(m);
                    }
                    else u1++;
                    break;
                case "Loja":
                    if (l1 != 0) {
                        Loja l = parseLoja(linhaPartida[1]);
                        m = r.getRegistos();
                        m.put(l.getCodigo(), l);
                        r.setRegistos(m);
                    }
                    else l1++;
                    break;
                case "Voluntario":
                    if (v1 != 0) {
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        m = r.getRegistos();
                        m.put(v.getCodigo(), v);
                        r.setRegistos(m);
                    }
                    else v1++;
                    break;
                case "Transportadora":
                    if (t1 != 0) {
                        Transportadora t = parseTransportadora(linhaPartida[1]);
                        m = r.getRegistos();
                        m.put(t.getCodigo(), t);
                        r.setRegistos(m);
                    }
                    else t1++;
                    break;
                case "Encomenda":
                    if (e1 != 0) {
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        r.adicionaEncomendaLU(e);
                    }
                    else e1++;
                    break;
                case "Aceite":
                    if (a1 != 0) r.aceitaEncomendaLU(linhaPartida[1]);
                    else a1++;
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Método que faz parsing de um Utilizador presente no ficheiro de Logs
     * @param input Linha com a informação do Utilizador
     * @return Utilizador
     */
    public Utilizador parseUtilizador(String input) {
        String[] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Utilizador u = new Utilizador();
        u.setMail(codUtilizador+"@kermit.com");
        u.setNome(nome);
        u.setCodigo(codUtilizador);
        Ponto p = new Ponto(gpsx, gpsy);
        u.setGps(p);
        u.setPassword(codUtilizador);
        return u;
    }

    /**
     * Método que faz parsing de uma Loja presente no ficheiro de Logs
     * @param input Linha com a informação da Loja
     * @return Loja
     */
    public Loja parseLoja(String input) {
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        Loja l = new Loja();
        l.setMail(codLoja+"@kermit.com");
        l.setCodigo(codLoja);
        l.setNome(nomeLoja);
        l.setPassword(codLoja);
        return l;
    }

    /**
     * Método que faz parsing de uma Voluntário presente no ficheiro de Logs
     * @param input Linha com a informação da Voluntário
     * @return Voluntário
     */
    public Voluntario parseVoluntario(String input) {
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        Voluntario v = new Voluntario();
        v.setMail(cod+"@kermit.com");
        v.setCodigo(cod);
        v.setNome(nome);
        Ponto p = new Ponto(gpsx, gpsy);
        v.setGps(p);
        v.setRaio(raio);
        v.setPassword(cod);
        return v;
    }

    /**
     * Método que faz parsing de uma Transportadora presente no ficheiro de Logs
     * @param input Linha com a informação da Transportadora
     * @return Transportadora
     */
    public Transportadora parseTransportadora(String input) {
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double preco = Double.parseDouble(campos[6]);
        Transportadora t = new Transportadora();
        t.setCodigo(cod);
        t.setNome(nome);
        Ponto p = new Ponto(gpsx, gpsy);
        t.setMail(cod+"@kermit.com");
        t.setGps(p);
        t.setNif(nif);
        t.setRaio(raio);
        t.setPrecoKm(preco);
        t.setPassword(cod);
        return t;
    }

    /**
     * Método que faz parsing de uma Encomenda presente no ficheiro de Logs
     * @param input Linha com a informação da Encomenda
     * @return Encomenda
     */
    public Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        String peso = campos[3];
        int size = campos.length, acc = 0, i = 4;
        Encomenda e = new Encomenda();
        e.setCodEncomenda(codEncomenda);
        e.setCodUtilizador(codUtilizador);
        e.setCodLoja(codLoja);
        e.setPeso(Double.parseDouble(peso));
        while (i<size) {
            LinhaEncomenda l = new LinhaEncomenda();
            while (acc < 4) {
                if (acc == 0) l.setCodProd(campos[i+acc]);
                else if (acc == 1) l.setDescricao(campos[i+acc]);
                else if (acc == 2) l.setQuantidade(Double.parseDouble(campos[i+acc]));
                else if (acc == 3) l.setValorUnitario(Double.parseDouble(campos[i+acc]));
                acc++;
            }
            Map<String,LinhaEncomenda> m = e.getLinha();
            m.put(l.getCodProd(),l);
            e.setLinha(m);
            i=i+acc;
            acc=0;
        }
        return e;
    }

    /**
     * Método que faz a leitura do ficheiro para uma List de Strings
     * @param nomeFich Nome do ficheiro a ler
     * @return List de Strings
     */
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }
}
