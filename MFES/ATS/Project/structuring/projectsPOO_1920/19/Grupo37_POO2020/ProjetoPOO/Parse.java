import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Parse{
    private GerirTrazAqui batatinhas;

    public GerirTrazAqui parse(){
        List<String> linhas = lerFicheiro("batatas.csv");
        String[] linhaPartida;
        this.batatinhas = new GerirTrazAqui();
        for(String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]);
                    System.out.println(u.toString());
                    this.batatinhas.addCliente(u.getEmail(),u.getPassword(),u.getNome(),u.getLocalizacao());
                    break;

                case "Loja":
                    Lojas l = parseLojas(linhaPartida[1]);
                    System.out.println(l.toString());
                    this.batatinhas.addLoja(l.getEmail(),l.getPassword(),l.getNome(),l.getLocalizacao(),l.getAtendimentohabitual());
                    break;

                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    System.out.println(v.toString());
                    this.batatinhas.addVoluntario(v.getEmail(),v.getPassword(),v.getNome(),v.getLocalizacao(),v.getRaio(),v.aceitoTransporteMedicamentos(),v.getVelocidadeEst());
                    break;

                case "Encomenda":
                    Encomendas e = parseEncomendas(linhaPartida[1]);
                    this.batatinhas.getLojas().get(e.getCodLoja()).addEncomenda(e);
                    this.batatinhas.getCliente().get(e.getCodUtil()).addEncomenda(e);
                    this.batatinhas.getCliente().get(e.getCodUtil()).setNdeEnc(this.batatinhas.getCliente().get(e.getCodUtil()).getNdeEnc() + 1);
                    break;

                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    System.out.println(t.toString());
                    this.batatinhas.addEmpresa(t.getEmail(),t.getPassword(),t.getNome(),t.getLocalizacao(),t.getRaio(),t.aceitoTransporteMedicamentos(),t.getAceite(),t.getPrecokms(),t.getPrecoTemp(),t.getVelocidadeEst());
                    break;
                
                case "Aceite":
                    aceitaEncomenda(linhaPartida[1]);
                    break;
                    
                default:
                    System.out.println("Linha invalida.");
                    break;
            }

        }
        System.out.println("done!");
        return batatinhas;
    }

    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codigo = campos[0];
        String nome = campos[1];
        GPS localizacao = new GPS(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
        Cliente c = new Cliente();
        c.setEmail(campos[0]);
        c.setPassword(campos[0] + "password");
        c.setNome(nome);
        c.setCodigo(codigo);
        c.setLocalizacao(localizacao);
        return c;
    }

    public Lojas parseLojas(String input){
        String[] campos = input.split(",");
        String codigo = campos[0];
        String nome= campos[1];
        GPS localizacao = new GPS(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
        Lojas l = new Lojas();
        l.setEmail(campos[0]);
        l.setPassword(campos[0] + "password");
        l.setLocalizacao(localizacao);
        l.setCodigo(codigo);
        l.setNome(nome);
        return l;
    }

    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codigo = campos[0];
        String nome = campos[1];
        GPS localizacao = new GPS(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
        float raio = Float.parseFloat(campos[4]);
        Voluntario v = new Voluntario();
        v.setEmail(campos[0]);
        v.setPassword(campos[0] + "password");
        v.setCodigoV(codigo);
        v.setNome(nome);
        v.setLocalizacao(localizacao);
        v.setRaio(raio);
        return v;
    }

    public Encomendas parseEncomendas(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        float peso = Float.parseFloat(campos[3]);
        Encomendas e = new Encomendas();
        e.setCodEnc(codEncomenda);
        e.setCodUtil(codUtilizador);
        e.setCodLoja(codLoja);
        e.setPeso(peso);
        //adicionar os produtos na encomenda
        int len = campos.length;
        int x;
        for(x = 4 ; x < len; x = x + 4){
            String inp = String.join(",",campos[x],campos[x+1],campos[x+2],campos[x+3]);
            e.addProduto(parseProduto(inp));
        }
        return e;
    }

    public Produto parseProduto(String input){
        String[] campos = input.split(",");
        String codProduto = campos[0];
        String descricao = campos[1];
        float quantidade = Float.parseFloat(campos[2]);
        float preco = Float.parseFloat(campos[3]);
        Produto p = new Produto();
        p.setReferencia(codProduto);
        p.setDescricao(descricao);
        p.setPeso(quantidade);
        p.setPreco(preco);
        return p;
    }

    public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codigo = campos[0];
        String nome = campos[1];
        GPS localizacao = new GPS(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
        float raio = Float.parseFloat(campos[5]);
        float precokms = Float.parseFloat(campos[6]);
        Transportadora t = new Transportadora();
        t.setEmail(campos[0]);
        t.setPassword(campos[0] + "password");
        t.setNome(nome);
        t.setCodigoT(codigo);
        t.setLocalizacao(localizacao);
        t.setRaio(raio);
        t.setPrecokms(precokms);
        return t;
    }
    
    public void aceitaEncomenda(String cod){
        String[] campos = cod.split(",");
        for (Lojas l: this.batatinhas.getLojas().values()){
            for (Encomendas e: l.getEncomenda()){
                if (e.getCodEnc().equals(campos[0])){
                    e.setDisponivel(true);
                    break;
                }
            }
        }
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage());}
        return lines;
    }
}