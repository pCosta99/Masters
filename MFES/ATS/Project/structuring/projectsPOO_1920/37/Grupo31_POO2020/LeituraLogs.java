import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.io.*;

public class LeituraLogs extends Menu implements Serializable{
    // função parse
    public void parse(String ficheiro, GestaoTotal gt){
        List<String> linhas = lerFicheiro(ficheiro); // ler input e passar input como string para o nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas){
            linhaPartida = linha.split(":",2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]);
                    gt.getGU().adicionaUtilizador(u);
                    //System.out.println(u.toString()); // enviar para o ecrã apenas para teste
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    gt.getGL().adicionaLoja(l);
                    //System.out.println(l.toString());
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    gt.getGV().adicionaVoluntario(v);
                    //System.out.println(v.toString());
                    break;
                case "Transportadora":
                    EmpresaTransportadora t = parseTransportadora(linhaPartida[1]);
                    gt.getGEMP().adicionaEmpresa(t);
                    //System.out.println(t.toString());;
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    gt.getGE().addEncomenda(e);
                    //System.out.println(e.toString());
                    break;
                case "Aceite":
                    String codEnc = linhaPartida[1];
                    gt.getGE().adicionaEncomendaAceite(codEnc,null);
                    //System.out.println("Leu linha válida de aceite");
                    break;
                default:
                    System.out.println("Linha inválida.");
                    break;
            }
        }
    }

    // dividir a linha do utilizador em campos
    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        // provavelmente tirar o arraylist?
        Localizacao local = new Localizacao(gpsx, gpsy);
        // (...) 
        return new Utilizador(codUtilizador + "@email.com", codUtilizador + "123", codUtilizador, local, nome);
    }

    // dividir a linha da loja em campos
    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Localizacao local = new Localizacao(gpsx, gpsy);
        // (...)
        return new Loja(local, codLoja, nomeLoja, codLoja + "@email.com", codLoja + "123");
    }

    // dividir a linha do voluntario
    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nomeVoluntario = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        //
        boolean estado = true;
        double velocidade = 0.0;
        Localizacao local = new Localizacao(gpsx, gpsy);
        boolean certificado = false;
        int numViagens = 0;
        double classificacao = 0.0;
        // (...)
        return new Voluntario(nomeVoluntario, estado, velocidade, local, raio, codVoluntario,certificado, numViagens, classificacao, codVoluntario + "@email.com", codVoluntario + "123");
    }

    // dividir a linha da transportadora
    public EmpresaTransportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nomeEmpresa = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double taxa = Double.parseDouble(campos[6]);
        //
        Localizacao local = new Localizacao(gpsx, gpsy);
        boolean estado = true;
        double custoTransporte = 0.0;
        int capacidadeEncomenda = 0;
        double tempoEntrega = 0.0;
        double classificacao = 0.0;
        double velocidade = 0.0;
        boolean certificado = false;
        int numViagens = 0;
        int numKms = 0;
        // (...)
        return new EmpresaTransportadora(nomeEmpresa, local, estado, custoTransporte, taxa,
                                            capacidadeEncomenda, tempoEntrega, classificacao, 
                                            velocidade, raio, codEmpresa, nif,certificado, numViagens,numKms,codEmpresa + "@email.com", codEmpresa + "123");
    }

    // dividir a linha da encomenda
    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        ArrayList<LinhaEncomenda> encomendas = new ArrayList<>();
        for (int i = 4; i + 4<= campos.length; i = i + 4) {
            String codProduto = campos[i];
            String descricao = campos[i+1];
            double quantidade = Double.parseDouble(campos[i+2]);
            double preco = Double.parseDouble(campos[i+3]);
            double imposto = 0.0;
            double desconto = 0.0;
            LinhaEncomenda l = new LinhaEncomenda(codProduto, descricao, preco, quantidade, imposto, desconto);
            encomendas.add(l);
        }

        LocalDate data = LocalDate.now();
        
        // (...)
        return new Encomenda(codUtilizador, codEncomenda, codLoja, data, peso, encomendas,"v00");
    }

    // leitura do ficheiro
    public List<String> lerFicheiro(String nomeFich){
        List<String> lines = new ArrayList<>();
        try{
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        }
        catch(IOException exc){
            System.out.println("Erro na leitura do ficheiro.");
        }
        return lines;
    }
}