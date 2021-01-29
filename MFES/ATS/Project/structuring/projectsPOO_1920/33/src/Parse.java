// nao sei se isto interessa, mas se esta aqui é pq sim XD
//import javax.rmi.CORBA.Util;
import java.awt.geom.Point2D;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public class Parse implements Serializable {
    Map<String, Utilizador> utilizadores = new HashMap<>();
    Map<String, Voluntario> voluntarios = new HashMap<>();
    Map<String, Loja> lojas = new HashMap<>();
    Map<String, Transportadora> transportadoras = new HashMap<>();
    Map<String, Encomenda> encomendas = new HashMap<>();
    List<String> aceites = new ArrayList<>();

    public void parse(String nomeFicheiro,Service servico) {
        List<String> linhas = lerFicheiro(nomeFicheiro);  //("src/logs.csv"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    insereUtilizador(u);
                    System.out.println(u.toString()); //enviar para o ecrã apenas para teste
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    insereVoluntario(v);
                    System.out.println(v.toString());
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    insereTransportadora(t);
                    System.out.println(t.toString());
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    insereLoja(l);
                    System.out.println(l.toString());
                    break;

                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    insereEncomenda(e);
                    System.out.println(e.toString());
                    break;

                case "Aceite":
                    String a = linhaPartida[1];
                    aceites.add(a);
                    System.out.println(a.toString());
                    break;
                default:
                    System.out.println("Linha inválida.");
                    break;
            }


            //

        }
        System.out.println("Ficheiro carregado!\n\n\n\n\n\n\n");
        // coloca uma encomnda em aceite
        corrigeEncomenda();
        //     addProdutosLoja();
        try {
            addProdutos();
        } catch (ProdutoRepetidoException | LojaInexistenteException e) {
            e.getMessage();
            System.out.println("produto nao existe ___________");
        }

        transformaEncomenda();
        servico.setVoluntarios(voluntarios);
        servico.setLojas(lojas);
        servico.setEncomendas(encomendas);
        servico.setTransportadoras(transportadoras);
        servico.setUtilizadores(utilizadores);

    }

    void transformaEncomenda(){
        Random rand = new Random();
        for(Encomenda e : this.encomendas.values()){
            if(e.getEstado()==1) {
                this.utilizadores.get(e.getCodUtilizador()).addEncomenda();
            }
            if(e.getEstado() == 2){
                this.utilizadores.get(e.getCodUtilizador()).addEncomenda();
                int r = rand.nextInt(2);
                int v = rand.nextInt(this.transportadoras.values().size());

                int vv = rand.nextInt(this.voluntarios.values().size());

                if(r==1) {
                    String cod = "no";
                    int i = 0;
                    for (Transportadora t : this.transportadoras.values()) {
                        if (i == v) cod = t.getCodigo();
                        i++;
                    }
                    this.transportadoras.get(cod).addEncomenda();
                }
                else {
                    String cod = "no";
                    int i = 0;
                    for (Voluntario t : this.voluntarios.values()) {
                        if (i == vv) cod = t.getCodigo();
                        i++;
                    }
                    this.voluntarios.get(cod).addEncomenda();
                }
            }
        }
    }

    void corrigeEncomenda() {
        for (String s : this.aceites) {
            if (this.encomendas.containsKey(s))
                this.encomendas.get(s).setEstado(2);
        }
    }

    void addEncomendas() {
        for (Encomenda e : this.encomendas.values()) {

            String loj = e.getCodLoja();
            String cli = e.getCodUtilizador();
            String enc = e.getCodEncomenda();

            if (e.getEstado() == 2) {
                if (this.utilizadores.containsKey(cli))
                    this.utilizadores.get(cli).addEncomenda();

                this.utilizadores.get(cli).addEncomenda();
            } else {

            }

        }
    }


    public void insereUtilizador(Utilizador u) {
        this.utilizadores.put(u.getCodigo(), u.clone());
    }

    public void insereVoluntario(Voluntario v) {
        this.voluntarios.put(v.getCodigo(), v.clone());
    }

    public void insereLoja(Loja l) {
        this.lojas.put(l.getCodigo(), l.clone());
    }

    public void insereTransportadora(Transportadora t) {
        this.transportadoras.put(t.getCodigo(), t.clone());
    }

    public void insereEncomenda(Encomenda e) {
        this.encomendas.put(e.getCodEncomenda(), e);
    }


    public Utilizador parseUtilizador(String input) {
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String email = criaEmail(codUtilizador);
        String password = criaPassword(codUtilizador);
        int nrEncomendas = createRandomIntBetween(1, 20);
        return new Utilizador(codUtilizador, nome, gpsx, gpsy, email, password, nrEncomendas);
    }

    public Voluntario parseVoluntario(String input) {
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nomeVoluntario = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        String email = criaEmail(codVoluntario);
        String password = criaPassword(codVoluntario);
        Classificacoes c = new Classificacoes();
        int nrEncomendas = createRandomIntBetween(1, 20);
        return new Voluntario(codVoluntario, nomeVoluntario, gpsx, gpsy, raio, email, password, c, nrEncomendas);
    }

    public Transportadora parseTransportadora(String input) {
        String[] campos = input.split(",");
        String codTransportadora = campos[0];
        String nomeTransportadora = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);
        String email = criaEmail(codTransportadora);
        String password = criaPassword(codTransportadora);
        Classificacoes c = new Classificacoes();
        int nrEncomendas = createRandomIntBetween(1, 20);
        double kms = doubleEntre(10000, 278000);

        return new Transportadora(codTransportadora, nomeTransportadora, gpsx, gpsy, nif, raio, precoKm, email, password, c, nrEncomendas, kms);
    }

    public Loja parseLoja(String input) {
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String email = criaEmail(codLoja);
        String password = criaPassword(codLoja);
        List<Produto> c = new ArrayList<>();
        return new Loja(codLoja, nomeLoja, gpsx, gpsy, email, password, c);
    }


    public Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        LinhaEncomenda ll = parseLinhasEncomenda(input);
        //  List<LinhaEncomenda> linhasEncomenda = parseLinhasEncomenda(input);
        LocalDate dataEncomenda = createRandomDate();
        int est = 1;
        return new Encomenda(codEncomenda, codUtilizador, codLoja, peso, ll, dataEncomenda, est,"");
    }

    public LinhaEncomenda parseLinhasEncomenda(String input) {
        List<Produto> produtos = new ArrayList<>();

        String[] campos = input.split(",");
        for (int i = 4; i < campos.length; i = i + 4) {
            String codProduto = campos[i];
            String nomeProduto = campos[i + 1];
            double quantidade = Double.parseDouble(campos[i + 2]);
            double valorUnitario = Double.parseDouble(campos[i + 3]);
            Random random = new Random();
            double peso =random.nextInt(15) +1.0;
            Produto p = new Produto(codProduto, nomeProduto, quantidade, valorUnitario, false,peso);
            produtos.add(p);
        }
        LinhaEncomenda linha1 = new LinhaEncomenda(produtos);

        return linha1;
    }

    public static double doubleEntre(int start, int end) {
        return start + (double) Math.round(Math.random() * (end - start));
    }

    public void addProdutosLoja(List<Produto> produtos, String codLoja) throws LojaInexistenteException, ProdutoRepetidoException {
        for (Produto p : produtos) {
            Produto novo = new Produto(p.getCodProduto(), p.getNomeProduto(), doubleEntre(20, 100), doubleEntre(1, 15), false,p.getPeso());
            lojas.get(codLoja).adicionaProduto(novo);
        }
    }


    public void addProdutos() throws ProdutoRepetidoException, LojaInexistenteException {
        for (Encomenda e : this.encomendas.values()) {
            addProdutosLoja(e.getProdutos(), e.getCodLoja());
        }
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }

    public String criaEmail(String id) {
        StringBuilder sb = new StringBuilder();
        sb.append(id).append("@trazaqui.pt");
        return sb.toString();
    }

    public String criaPassword(String id) {
        StringBuilder sb = new StringBuilder();
        sb.append("1234").append(id);
        return sb.toString();
    }


    public static int createRandomIntBetween(int start, int end) {
        return start + (int) Math.round(Math.random() * (end - start));
    }

    public static LocalDate createRandomDate() {
        int day = createRandomIntBetween(1, 28);
        int month = createRandomIntBetween(1, 5);
        int year = createRandomIntBetween(2020, 2020);
        return LocalDate.of(year, month, day);
    }


    public double distanciaLojaTransportadora(String codLoja, String codTransportadora) {
        Loja l = lojas.get(codLoja);
        Transportadora t = transportadoras.get(codTransportadora);
        Point2D.Double coordLoja = new Point2D.Double(l.getGpsx(), l.getGpsy());
        Point2D.Double coordTransportadora = new Point2D.Double(t.getGpsx(), t.getGpsy());
        double distancia = coordLoja.distance(coordTransportadora);
        return distancia;
    }

}


