import java.lang.String;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Arrays;
import java.io.*;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.stream.Collectors;

public class Parse{
    
    public GestaoUsuarios carregaEstado() throws IOException{
        List<String> linhas=lerFicheiro("Logs.csv");
        String[] campos;
        GestaoUsuarios gu = new GestaoUsuarios();
        String[] linhaDePartida;
        List<Encomenda> encomendas = new ArrayList<>();
        List<String> encomendasAceites = new ArrayList<>();
        List<EncomendaEfetuada> encomendasEfetuadas = new ArrayList<>();
        Map<String,String[]> dadosLogin = new HashMap<>(); 
        String[] dados;
        
        for (String linha: linhas){
            linhaDePartida = linha.split(":");
            switch(linhaDePartida[0]){
                case "Utilizador":
                    gu.addUsuario(parseUtilizador(linhaDePartida[1]));
                    break;
                case "Voluntario":
                    gu.addUsuario(parseVoluntario(linhaDePartida[1]));
                    break;
                case "Transportadora":
                    gu.addUsuario(parseEmpresaTransportadora(linhaDePartida[1]));
                    break;
                case "Loja":
                    gu.addUsuario(parseLoja(linhaDePartida[1]));
                    break;
                case "Encomenda":
                    encomendas.add(parseEncomenda(linhaDePartida[1]));
                    break;
                case "Aceite":
                    encomendasAceites.add(linhaDePartida[1]);
                    break;
                case "Login"://<codU>,<mail>,<pass>
                    campos=linhaDePartida[1].split(",");
                    dados=new String[2];
                    dados[0]=campos[1];
                    dados[1]=campos[2];
                    dadosLogin.put(campos[0],dados);
                    break;
                case "Encomenda efetuada"://<codEnc>,<codUB>,<codL>,<codT>,<Peso>,<cudto entrega>,<data de entrega>,<Linhas>
                    encomendasEfetuadas.add(parseEncEfe(linhaDePartida[1]));
                    break;
                default:
                    break;
            }
            
        }
        gu.addEncomendas(encomendas,encomendasAceites);
        gu.addEncomendasEfetuadas(encomendasEfetuadas);
        gu.addDadosLogin(dadosLogin);
        return gu;
    }
    
    public UtilizadorBasico parseUtilizador(String input){
        String[] campos = input.split(",");
        return new UtilizadorBasico(new GPS(Float.parseFloat(campos[2]),Float.parseFloat(campos[3])),null,null,
                    new GestaoEncomendas(),new GestaoEncomendas(),campos[1],campos[0],new ArrayList<>(), new HashMap<>()
                    ,new ArrayList<>());
    }
    
    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        return new Loja (campos[0],campos[1],new GPS(Float.parseFloat(campos[2]),Float.parseFloat(campos[3]))
                                    ,null,null,new GestaoEncomendas(),new GestaoEncomendas(),new ArrayList<>()
                                    ,new ArrayList<>(),new HashMap<>());
        
    }
    
    public EmpresaTransportadora parseEmpresaTransportadora(String input){
        String[] campos = input.split(",");
        return new EmpresaTransportadora(new GPS(Float.parseFloat(campos[2]),Float.parseFloat(campos[3])),null, 
                        null,campos[1],campos[0],Float.parseFloat(campos[5]),false,new GestaoEncomendas(),
                        new GestaoEncomendas(),Integer.parseInt(campos[4]),Float.parseFloat(campos[6]),-1,new GestaoEncomendas(),new ArrayList<>()
                        ,new HashMap<>(),0,new HashMap<>());
    }
    
    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        return new Voluntario(new GPS(Float.parseFloat(campos[2]),Float.parseFloat(campos[3])),null,null,
                       campos[1],campos[0], Float.parseFloat(campos[4]),false,new GestaoEncomendas(),new GestaoEncomendas(),
                       new HashMap<>(),false,0);
    }
    
    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        Encomenda enc = new EncomendaBasica(campos[0],campos[1],campos[2],Float.parseFloat(campos[3]),new HashMap<>());
        for(int i=4;i<campos.length;i+=4){
            enc.addLEncomenda(parseLinhaEncomenda(Arrays.copyOfRange(campos,i,i+4)));
        }
        return enc;
    }
    
    public LinhaEncomenda parseLinhaEncomenda(String input){
        String[] campos = input.split(",");
        return new LinhaEncomenda(campos[0],campos[1],Float.parseFloat(campos[2]),Float.parseFloat(campos[3]),0);
    }
    
    public LinhaEncomenda parseLinhaEncomenda(String[] campos){
        return new LinhaEncomenda(campos[0],campos[1],Float.parseFloat(campos[2]),Float.parseFloat(campos[3]),0);
    }
    
    //(String codE, String codU, String codL,codT,float peso,float custo, Map<String,LinhaEncomenda> prod,LocalDateTime date)
    public EncomendaEfetuada parseEncEfe(String input){
        String[] campos = input.split(",");
        EncomendaEfetuada enc = new EncomendaEfetuada(campos[0],campos[1],campos[2],campos[3],Float.parseFloat(campos[4]),
                        Float.parseFloat(campos[5]),new HashMap<>(),this.parseTempo(campos[6]));
        for(int i=7;i<campos.length;i+=4){
            enc.addLEncomenda(parseLinhaEncomenda(Arrays.copyOfRange(campos,i,i+4)));
        }
        return enc;
    }
    
    public List<String> lerFicheiro(String nomeFich) throws IOException{
        List<String> lines= new ArrayList<>();
        try {lines=Files.readAllLines(Paths.get(nomeFich),StandardCharsets.UTF_8);}
        catch (IOException exc) {System.out.println("Erro a ler : "+exc.getMessage());}
        return lines;
    }
    
    public void guardarEstado(GestaoUsuarios gu) throws IOException{
        guardarEstado("Logs.csv",gu);
    }
    
    public void guardarEstado(String fileName,GestaoUsuarios gu) throws IOException{
        
        PrintWriter state = new PrintWriter(fileName);
        StringBuilder sb=new StringBuilder();
        sb.append("-----------------------TrazAqui-----------------------\n\n")
        .append("timestamp:").append(LocalDateTime.now().toString()).append("\n")
        .append("\n==============================================================================================\n")
        .append("Dados de LOGS:")
        .append("\n==============================================================================================\n\n");
       
        sb.append(gu.paraEstado()).append(gu.getEncomendasAceites()).
        append(gu.getEncomendasEfetuadas()).append(gu.getDadosLogin());

        state.print(sb.toString());
        
        state.flush();
        state.close();
    }
    
    public LocalDateTime parseTempo(String time){
        
            String campos[]=time.split(";");
            int dia = Integer.parseInt(campos[0]);
            int mes = Integer.parseInt(campos[1]);
            int ano = Integer.parseInt(campos[2]);
            int hora = Integer.parseInt(campos[3]);
            int minutos = Integer.parseInt(campos[4]);
            return LocalDateTime.of(ano,mes,dia,hora,minutos);
        
    }
    
    public static String timeStr(LocalDateTime t){
        StringBuilder sb=new StringBuilder();
        sb.append(String.valueOf(t.getDayOfMonth())).append(";").
        append(String.valueOf(t.getMonthValue())).append(";").
        append(String.valueOf(t.getYear())).append(";").
        append(String.valueOf(t.getHour())).append(";").
        append(String.valueOf(t.getMinute()));
        return sb.toString();
    }
    
}

























