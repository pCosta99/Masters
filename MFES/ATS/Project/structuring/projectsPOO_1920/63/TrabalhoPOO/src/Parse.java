import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public class Parse {
    /** variaveis de instancia */

    /** constructores de classe */
    /** vazio */

    /** parametrico */

    /** copia */

    /** gets/sets das variaveis de instancia */

    /** metodos override */

    /** metodos especificos */
    //-------------------------LEITURA-------------------------//
    /**
     * Faz a leitura do ficheiro
     */
    public void parse(Model m, String path, int flag){
        List<String> linhas = lerFicheiro(path); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1],flag, m); // criar um Utilizador
                    //System.out.println(u.toString()); //enviar para o ecrán apenas para teste
                    m.insereUtilizador(u);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1], flag); // criar um Voluntario
                    //System.out.println(v.toString()); //enviar para o ecrán apenas para teste
                    m.insereVoluntario(v);
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1], flag); // criar um Transportadora
                    //System.out.println(t.toString()); //enviar para o ecrán apenas para teste
                    m.insereTransportadora(t);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1], flag, m); // criar uma Loja
                    //System.out.println(l.toString()); //enviar para o ecrán apenas para teste
                    m.insereLoja(l);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1], m); // criar uma Encomenda
                    //System.out.println(e.toString()); //enviar para o ecrán apenas para teste
                    m.insereEncomenda(e);
                    break;
                case "Aceite":
                    String a = linhaPartida[1];
                    //System.out.println("Aceite:" + a); //enviar para o ecrán apenas para teste
                    m.insereAceite(a);
                    break;
                default:
                    //System.out.println("Linha inválida.");
                    break;
            }
        }
        System.out.println("Ficheiro carregado com sucesso!");
    }

    /**
     * Faz a leitura de um utilizador do ficheiro de texto.
     * @param input contem a String com os campos do utilizador
     * @return
     */
    public Utilizador parseUtilizador(String input, int flag, Model m){
        String[] campos = input.split(",");

        //variaveis a preencher
        String codUtilizador;
        String nome;
        double gpsx;
        double gpsy;
        String email;
        String password;

        //variaveis comuns
        codUtilizador = campos[0];
        nome = campos[1];
        gpsx = Double.parseDouble(campos[2]);
        gpsy = Double.parseDouble(campos[3]);

        if(flag==0){
            email = codUtilizador + "@missing.com";
            password = codUtilizador + "@missing.com";
        }else{
            email = campos[4];
            password = campos[5];
        }

        //verifica se utilizador já existe
        Utilizador u = new Utilizador(codUtilizador, nome, gpsx, gpsy, email, password);
        if(!m.getUtlizadores().contains(u))
            m.insereUtilizador(u);

        return u;
    }

    /**
     * Faz a leitura de um voluntario do ficheiro de texto.
     * @param input contem a String com os campos do voluntario
     * @param flag indica de provem do log incial (0) ou de um log posterior (1)
     * @return
     */
    public Voluntario parseVoluntario(String input, int flag){
        String[] campos = input.split(",");

        //variaveis a preencher
        String codVoluntario;
        String nome;
        double gpsX;
        double gpsY;
        String email;
        String password;
        double raio;
        boolean disponibilidade;

        //variaveis comuns
        codVoluntario = campos[0];
        nome = campos[1];
        gpsX = Double.parseDouble(campos[2]);
        gpsY = Double.parseDouble(campos[3]);

        if(flag==0){
            email = codVoluntario + "@missing.com";
            password = codVoluntario + "@missing.com";
            raio = Double.parseDouble(campos[4]);
            disponibilidade = true;
        }else{
            email = campos[4];
            password = campos[5];
            raio = Double.parseDouble(campos[6]);
            disponibilidade = Boolean.parseBoolean(campos[7]);
        }

        return new Voluntario(codVoluntario, nome, gpsX, gpsY, email, password,
                raio, disponibilidade);
    }

    /**
     * Faz a leitura de uma transportadora do ficheiro de texto.
     * @param input contem a String com os campos da transportadora
     * @param flag indica de provem do log incial (0) ou de um log posterior (1)
     * @return
     */
    public Transportadora parseTransportadora(String input, int flag){
        String[] campos = input.split(",");

        //variaveis a preencher
        String codTransportadora;
        String nome;
        double gpsX;
        double gpsY;
        String email;
        String password;
        String nif;
        double raio;
        double precoPorKm;
        double precoPorMin;
        boolean disponibilidade;
        List<DadosEntrega> historico = new ArrayList<>();

        //variaveis comuns
        codTransportadora = campos[0];
        nome = campos[1];
        gpsX = Double.parseDouble(campos[2]);
        gpsY = Double.parseDouble(campos[3]);

        if(flag==0){
            nif = campos[4];
            raio = Double.parseDouble(campos[5]);
            precoPorKm = Double.parseDouble(campos[6]);
            email = codTransportadora + "@missing.com";
            password = codTransportadora + "@missing.com";
            precoPorMin = precoPorKm/5;
            disponibilidade = true;
        }else{
            email = campos[4];
            password = campos[5];
            nif = campos[6];
            raio = Double.parseDouble(campos[7]);
            precoPorKm = Double.parseDouble(campos[8]);
            precoPorMin = Double.parseDouble(campos[9]);
            disponibilidade = Boolean.parseBoolean(campos[7]);
        }

        return new Transportadora(codTransportadora, nome, gpsX, gpsY, email, password, nif, raio,
                precoPorKm, precoPorMin, disponibilidade, historico);
    }

    /**
     * Faz a leitura de uma loja do ficheiro de texto.
     * @param input contem a String com os campos da loja
     * @param flag indica de provem do log incial (0) ou de um log posterior (1)
     * @return
     */
    public Loja parseLoja(String input, int flag, Model m){
        String[] campos = input.split(",");

        //variaveis a preencher
        String codLoja;
        String nome;
        double gpsX;
        double gpsY;
        String email;
        String password;
        String nif;

        //variaveis comuns
        codLoja = campos[0];
        nome = campos[1];

        if(flag==0){
            //preenche gps que nao existia no ficheiro
            List<Double> l = new ArrayList<>();
            for(int i=0 ; i<2 ; i++){
                double auxSignal = Math.random();
                double auxValue = Math.random();
                if(auxSignal<0){
                    auxValue = auxValue * -100;
                }else{
                    auxValue = auxValue * 100;
                }
                l.add(i,auxValue);
            }
            gpsX = l.get(0);
            gpsY = l.get(1);

            //preenche nif que nao existia no ficheiro
            StringBuilder newNif = new StringBuilder();
            for(int i=0 ; i<9 ; i++){
                int aux = (int) Math.round(9*Math.random());
                newNif.append(Integer.toString(aux));
            }
            nif = newNif.toString();
            email = codLoja + "@missing.com";
            password = codLoja + "@missing.com";
        }else{
            //prrenche gps e nif que existem no ficheiro
            gpsX = Double.parseDouble(campos[2]);
            gpsY = Double.parseDouble(campos[3]);
            email = campos[4];
            password = campos[5];
            nif = campos[6];
        }

        //verifica se loja já existe
        Loja loja = new Loja(codLoja, nome, gpsX, gpsY, email, password, nif);
        if(!m.getLojas().contains(loja))
            m.insereLoja(loja);

        return loja;
    }

    /**
     * Faz a leitura de uma encomenda do ficheiro de texto.
     * @param input contem a String com os campos da encomenda
     * @return
     */
    public Encomenda parseEncomenda(String input, Model m){
        String[] campos = input.split(",",5);
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);

        List<LinhaEncomenda> linhasEncomenda = new ArrayList<>();
        String codProduto;
        String descricao;
        double precoUnitario;
        double quantidade;

        StringTokenizer aux = new StringTokenizer(campos[4], ",");
        while (aux.hasMoreElements()) {
            codProduto = aux.nextToken();
            descricao = aux.nextToken();
            quantidade = Double.parseDouble(aux.nextToken());
            precoUnitario = Double.parseDouble(aux.nextToken());
            linhasEncomenda.add(new LinhaEncomenda(codProduto, descricao, quantidade, precoUnitario));
        }

        return new Encomenda(codEncomenda, m.devolveUtilizador(codUtilizador),
                m.devolveLoja(codLoja), peso, linhasEncomenda);
    }

    /**
     * Faz a leitura do ficheiro de texto.
     * @param nomeFich contem a String com o path para o ficheiro a ler
     * @return
     */
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }

    //-------------------------GRAVACAO-------------------------//
    /**
     * Exportar a informação do model para um ficheiro txt.
     */
    public void exportaViculos(Model m){
        try (PrintWriter writer = new PrintWriter(
                new File("Log" + LocalDateTime.now() +".txt"))) {
            writer.write(m.toString());
            System.out.println("Ficheiro gravado com sucesso!\n");
        } catch (FileNotFoundException e) {
            System.out.println("Erro ao gravar o ficheiro!\n");
        }
    }

}
