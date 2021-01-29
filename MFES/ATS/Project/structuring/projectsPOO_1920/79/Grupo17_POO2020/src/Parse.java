

import java.time.LocalDateTime;
import java.util.*;
import java.io.*;
import java.util.function.Predicate;

public class Parse implements Serializable
{

    /** conjunto de entidades do ficheiro logs */
    private Map<String,Entidade> entidades;

    /** conjunto de encomendas do ficheiro logs */
    private Map<String,Encomenda> encomendas;

    /** conjunto de identificadores das encomendas 'aceite' do ficheiro logs */
    private List<String> encAceites;


    /**
     * Contrutor por omissão de parde
     */
    public Parse(){
        this.entidades = new TreeMap<>();
        this.encomendas = new TreeMap<>();
        this.encAceites = new ArrayList<>();
    }

    /**
     * Getter do conjunto de entidades do ficheiro logs
     * @return conjunto de entidades do ficheiro logs
     */
    public Map<String,Entidade> getEntidades(){
        Map<String,Entidade> aux = new TreeMap<>();
        for(Map.Entry<String,Entidade> e : this.entidades.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    /**
     * Getter do conjunto de encomendas do ficheiro logs
     * @return conjunto de encomendas do ficheiro logs
     */
    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> aux = new TreeMap<>();
        for(Map.Entry<String,Encomenda> e : this.encomendas.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    /**
     * Getter do conjunto de identificadores das encomendas 'aceite' do ficheiro logs
     * @return conjunto de identificadores das encomendas 'aceite' do ficheiro logs
     */
    public List<String> getEncomendasAceites(){
        List<String> aux = new ArrayList<>();
        for(String e : this.encAceites) aux.add(e);
        return aux;
    }

    /**
     * Metodo que adiciona uma entidade ao conjunto de entidades do ficheiro logs
     * @param e entidade a inserir
     */
    public void addEntidade(Entidade e){this.entidades.put(e.getCodigo(),e.clone());}

    /**
     * Metodo que adiciona uma encomenda ao conjunto de encomendas do ficheiro logs
     * @param e encomenda a inserir
     */
    public void addEncomenda(Encomenda e){this.encomendas.put(e.getCodEncomenda(),e.clone());}

    /**
     * Metodo que adiciona o codigo da encomenda a inserir no encAceites
     * @param codEnc codigo da encomenda a inserir
     */
    public void addEncomendaAceite(String codEnc){this.encAceites.add(codEnc);}

    /**
     * Metodo que lê o ficheiro logs e adiciona as entidades e encomendas as estruturas de dados corretas
     * @param filename nome do ficheiro a ler
     */
    public void parse(String filename){
        List<String> linhas = readFile(filename);
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    addEntidade(u);
                    break;

                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    addEntidade(l);
                    break;

                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    addEntidade(t);
                    break;

                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    addEntidade(v);
                    break;

                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    addEncomenda(e);
                    break;

                case "Aceite":
                    addEncomendaAceite(linhaPartida[1]);
                    break;

                default:
                    System.out.println();
                    break;
            }
        }
    }

    /**
     * Metodo que cria um utlizador a partir de uma linha do ficheiro logs
     * @param input linha do ficheiro logs
     * @return utilizador
     */
    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Utilizador u = new Utilizador(nome,codUtilizador,gpsx,gpsy,new TreeMap<String,Encomenda>(),codUtilizador+"@gmail.com",codUtilizador+"123");
        return u;
    }

    /**
     * Metodo que cria uma loja a partir de uma linha do ficheiro logs
     * @param input linha do ficheiro logs
     * @return loja
     */
    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Loja l = new Loja(codLoja,nomeLoja,gpsx,gpsy,new TreeMap<String,Encomenda>(),codLoja+"@gmail.com",codLoja+"123");
        return l;
    }

    /**
     * Metodo que cria uma transportadora a partir de uma linha do ficheiro logs
     * @param input linha do ficheiro logs
     * @return transportadora
     */
    public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codTransportadora = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double precoPorKm = Double.parseDouble(campos[6]);
        Transportadora t = new Transportadora(codTransportadora,nome,nif,gpsx,gpsy,precoPorKm,raio,1,false,new TreeMap<String,Encomenda>(),codTransportadora+"@gmail.com",codTransportadora+"123",0,0,0);
        return t;
    }

    /**
     * Metodo que cria um voluntario a partir de uma linha do ficheiro logs
     * @param input linha do ficheiro logs
     * @return voluntario
     */
    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        Voluntario v = new Voluntario(nome,codVoluntario,gpsx,gpsy,raio,false,new TreeMap<String,Encomenda>(),codVoluntario+"@gmail.com",codVoluntario+"123",0,0,0,true,new String(),new Encomenda(),new String(), LocalDateTime.of(2000,1,1,0,0),LocalDateTime.of(2000,1,1,0,0));
        return v;
    }

    /**
     * Metodo que cria uma encomenda a partir de uma linha do ficheiro logs
     * @param input linha do ficheiro logs
     * @return encomenda
     */
    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        int p=4, s=5, t=6, q=7, r = ((campos.length-4)/4);
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double pesoEnc = Double.parseDouble(campos[3]);
        Map<String,LinhaEnc> produtos = new TreeMap<>();
        while(r>0){
            LinhaEnc l = parseLinhaEnc(p,s,t,q,campos);
            String k = campos[p];
            produtos.put(k,l.clone());
            p+=4;
            s+=4;
            t+=4;
            q+=4;
            r--;
        }
        return new Encomenda(codUtilizador,codLoja,pesoEnc,produtos,codEncomenda,0,0,0,0,0,0);
    }

    /**
     * Metodo que cria uma linha de encomenda a partir de uma parte de uma linha do ficheiro logs
     * @param p identificador do produto
     * @param s decrição do produto
     * @param t quantidade do produto
     * @param q preço unitário do produto
     * @param campos parte de uma linha do ficheiro logs
     * @return linha de encomenda
     */
    public LinhaEnc parseLinhaEnc(int p,int s,int t,int q,String[] campos){
        String codProduto = campos[p];
        String descricao = campos[s];
        double quantidade = Double.parseDouble(campos[t]);
        double precoUni = Double.parseDouble(campos[q]);
        return new LinhaEnc(codProduto,descricao,quantidade,precoUni,1,false);
    }


    /**
     * Metodo que lê um fichiero
      * @param filePath nome do ficheiro
     * @return lista de linhas do ficheiro
     */
    public List<String> readFile(String filePath){
        List<String> lines = new ArrayList<>();

        try{
            BufferedReader br = new BufferedReader(new FileReader(filePath));
            String line = br.readLine();

            while(line!=null){
                lines.add(line);
                line = br.readLine();
            }
        } catch (IOException e){
            e.printStackTrace();
        }
        return lines;
    }

    /**
     * Metodo toString de parse
     * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append("Entidades do Ficheiro:\n").append(this.entidades).append("\n")
                .append("Encomendas do Ficheiro:\n").append(this.encomendas).append("\n")
                .append("Encomendas Aceites:\n").append(this.encAceites).append("\n");

        return s.toString();
    }

    /**
     * Predicado que verifica se uma encomenda está no 'encAceites'
     */
    Predicate<Encomenda> p1 = e -> !this.encAceites.contains(e.getCodEncomenda());
    /**
     * Preciado que verifica se uma encomenda está no 'encomendas'
     */
    Predicate<Encomenda> p2 = e -> this.encomendas.containsKey(e.getCodEncomenda());

    /**
     * Metodo que verifica se uma encomenda do ficheiro logs já foi aceite e entregue ao destinatário
     * @param e encomenda a verificar
     * @return booleano
     */
    public boolean encEntregue(Encomenda e){return p1.test(e) && p2.test(e);}

}