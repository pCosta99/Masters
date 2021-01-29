
import java.io.*;
import java.util.*;

public class TrazAqui implements Serializable{

    private Menu menu = new Menu();
    private Input input = new Input();
    private Dados dados = new Dados();
    private Parser parser = new Parser();

    public TrazAqui() throws Exception{
        carregarDados();
        inicio();
        guardarDados();
    }
    
    public static void main(String[] args) throws Exception{
        new TrazAqui();
    }

    public void inicio() throws Exception{
        int op = -1;
        while(op!=0) {
            menu.menuInicial();
            op = input.readInt();
            switch (op) {
                case 1:
                    logIn();
                    break;
                case 2:
                    registar();
                    break;
                default:
                    break;
            }
        }
    }

    public void logIn() throws Exception{
        menu.logIn();
        String email,password;
        int op = input.readInt();
        switch (op){
            case 1:
                System.out.println("******* Log In *******");
                System.out.println("Email: ");
                email = input.readString();
                System.out.println("Password: ");
                password = input.readString();
                try{
                    dados.logUtilizador(email,password);
                    menuUtilizador(email);
                }catch (LogInException e){
                    System.out.println(e.getMessage());
                    System.out.println("Insere 0 para avançar");
                    input.readString();
                }
                break;
            case 2:
                System.out.println("******* Log In *******");
                System.out.println("Email: ");
                email = input.readString();
                System.out.println("Password: ");
                password = input.readString();
                try{
                    dados.logLoja(email,password);
                    menuLoja(email);
                }catch (LogInException e){
                    System.out.println(e.getMessage());
                    System.out.println("Introduz 0 para avançar");
                    input.readString();
                }
                break;
            case 3:
                System.out.println("******* Log In *******");
                System.out.println("Email: ");
                email = input.readString();
                System.out.println("Password: ");
                password = input.readString();
                try{
                    int ins = dados.logTransporte(email,password);
                    if(ins==1) {
                        menuVoluntario(email);
                    }
                    else{
                        menuEmpresa(email);
                    }
                }catch (LogInException e){
                    System.out.println(e.getMessage());
                    System.out.println("Introduz 0 para avançar");
                    input.readString();
                }
                break;
            default:
                break;

        }
    }

    public void registar(){
        menu.registar();
        String nome,email,password;
        float pos_x, pos_y;
        int raio;
        int op = input.readInt();
        switch (op){
            case 1:
                menu.clear();
                System.out.println("Nome: ");
                nome = input.readString();
                System.out.println("Email: ");
                email = input.readString();
                System.out.println("Password: ");
                password = input.readString();
                System.out.println("Pos x: ");
                pos_x = input.readFloat();
                System.out.println("Pos y: ");
                pos_y = input.readFloat();
                try{
                    dados.registarUtilizador(email,nome,password,pos_x,pos_y);
                }catch (RegistarException e){
                    System.out.println(e.getMessage());
                    System.out.println("Introduz 0 para avançar");
                    input.readString();
                }
                break;
            case 2:
                menu.clear();
                System.out.println("Nome: ");
                nome = input.readString();
                System.out.println("Email: ");
                email = input.readString();
                System.out.println("Password: ");
                password = input.readString();
                System.out.println("Pos x: ");
                pos_x = input.readFloat();
                System.out.println("Pos y: ");
                pos_y = input.readFloat();
                try{
                    dados.registarLoja(email,nome,password,pos_x,pos_y);
                }catch (RegistarException e){
                    System.out.println(e.getMessage());
                    System.out.println("Introduz 0 para avançar");
                    input.readString();
                }
                break;
            case 3:
                menu.clear();
                System.out.println("Nome: ");
                nome = input.readString();
                System.out.println("Email: ");
                email = input.readString();
                System.out.println("Password: ");
                password = input.readString();
                System.out.println("Pos x: ");
                pos_x = input.readFloat();
                System.out.println("Pos y: ");
                pos_y = input.readFloat();
                System.out.println("Raio: ");
                raio = input.readInt();
                System.out.println("Taxa de entrega: ");
                float custo = input.readFloat();
                try{
                    dados.registarEmpresa(email,nome,password,pos_x,pos_y,raio,custo);
                }catch (RegistarException e){
                    System.out.println(e.getMessage());
                    System.out.println("Introduz 0 para avançar");
                    input.readString();
                }
                break;
            case 4:
                menu.clear();
                System.out.println("Nome: ");
                nome = input.readString();
                System.out.println("Email: ");
                email = input.readString();
                System.out.println("Password: ");
                password = input.readString();
                System.out.println("Pos x: ");
                pos_x = input.readFloat();
                System.out.println("Pos y: ");
                pos_y = input.readFloat();
                System.out.println("Raio: ");
                raio = input.readInt();
                try{
                    dados.registarVoluntario(email,nome,password,pos_x,pos_y,raio);
                }catch (RegistarException e){
                    System.out.println(e.getMessage());
                    System.out.println("Introduz 0 para avançar");
                    input.readString();
                }
                break;
            default:
                break;
        }
    }

    public void menuUtilizador(String email) throws Exception{
        int op = -1;
        while(op != 0){
            menu.utilizador(email);
            op = input.readInt();
            switch (op){
                case 1:
                    efectuarEncomenda(email);
                    break;
                case 2:
                    encomendasPendentes(email);
                    break;
                case 3:
                    aceitarTransporte(email);
                    break;
                case 4:
                    historicoEncomendas(email);
                    break;
                case 5:
                    classificarEntrega(email);
                default:
                    break;
            }
        }
    }

    public void menuLoja(String email) throws Exception{
        int op=-1;
        while(op!=0){
            menu.loja(email);
             op = input.readInt();
            switch (op){
                case 1:
                    encomendasPendentesLoja(email);
                    break;
                case 2:
                    encomendasEsperaEntrega(email);
                    break;
                case 3:
                    historicoEncomendasLoja(email);
                    break;
                default:
                    break;
            }
        }
    }

    public void menuVoluntario(String email) throws Exception{
        int op=-1;
        while(op!=0){
            menu.voluntario(email);
             op = input.readInt();
            switch (op){
                case 1:
                    encomendasParaEntrega(email);
                    break;
                case 2:
                    historicoViagens(email);
                    break;
                default:
                    break;
            }
        }
    }

    public void menuEmpresa(String email) throws Exception{
        int op=-1;
        while(op!=0){
            menu.empresa(email);
             op = input.readInt();
            switch (op){
                case 1:
                    encomendasParaEntrega(email);
                    break;
                case 2:
                    historicoViagens(email);
                    break;
                case 3:
                    alterarCusto(email);
                    break;
                default:
                    break;
            }
        }    
    }

    public void efectuarEncomenda(String email) throws Exception{
        String loja, op = new String();
        String id = new String();
        List<String> artigos = new ArrayList<>();

        menu.clear();
        dados.printLojas();
        System.out.println("Introduza o id da Loja");
        loja = input.readString();
        try {
            dados.printArtigos(loja);
            System.out.println("Introduza o id dos artigos (0 para terminar)");

            while (!op.equals("0")) {
                op = input.readString();
                if (!op.equals("0")) {
                    artigos.add(op);
                }
            }
            try {
                dados.adicionarEncomenda(id, email, loja, artigos);
            } catch (LojaInvalidaException e) {
                System.out.println(e.getMessage());
                System.out.println("Introduz 0 para avançar");
                input.readString();
            }
        }catch (Exception e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para avançar");
            input.readString();
        }
     }

    public void encomendasPendentes(String email) throws EncomendaInvalidaException{
        try {
            menu.clear();
            System.out.println("À espera de aprovação da loja: ");
            dados.printEncomendas(0, email, 0);
            System.out.println("À espera de transporte: ");
            dados.printEncomendas(0, email, 1);
            System.out.println("Introduz 0 para voltar ao menu");
            input.readString();
        }catch (EncomendaInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para avançar");
            input.readString();
        }
    }
     
    public void aceitarTransporte(String email) throws Exception{
        try {
            menu.clear();
            System.out.println("Encomendas para aprovar transporte: ");
            dados.printTransporteAceitar(email);
            System.out.println("Introduz o id da encomenda para aceitar");
            String idEncomenda = input.readString();
            System.out.println("Introduz o id do transporte para aceitar");
            String idTransporte = input.readString();

            dados.aceitarTransporte(email, idEncomenda, idTransporte);
        }catch(EncomendaInvalidaException | TransporteInvalidoException e){
               System.out.println(e.getMessage());
         }
        System.out.println("Introduz 0 para voltar ao menu");
        input.readString();

    }
    public void historicoEncomendas(String email)throws EncomendaInvalidaException{
        try {
            menu.clear();
            dados.printEncomendas(0, email, 3);
            dados.printEncomendas(0, email, 4);
            System.out.println("Introduz 0 para voltar ao menu");
            input.readString();
        }catch (EncomendaInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para voltar ao menu");
            input.readString();
        }
    }

    public void classificarEntrega(String email) throws EncomendaInvalidaException{
        try{
            dados.printEncomendas(0,email,3);
            System.out.println("Id da encomenda a classificar: ");
            String idEncomenda = input.readString();
            System.out.println("Classificação (entre 0 e 5): ");
            int classificacao = input.readInt();

            dados.classificarEntrega(email, idEncomenda,classificacao);
        }catch(EncomendaInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para avançar");
            input.readString();
        }
    }
    
    public void encomendasPendentesLoja(String email) throws Exception{
        try{
            dados.printEncomendas(1,email,0);
            System.out.println("Colocar id encomenda para aceitar: ");
            String idEncomenda = input.readString();

            dados.aceitarEncomenda(email,idEncomenda);
        }catch (EncomendaInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para avançar");
            input.readString();
        }
    }
    
    
    public void encomendasEsperaEntrega(String email) throws EncomendaInvalidaException{
        try{
            dados.printEncomendas(1,email,1);
            input.readString();
        }catch (EncomendaInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para avançar");
            input.readString();
        }
    }
   
    
    public void historicoEncomendasLoja(String email) throws EncomendaInvalidaException{
        try {
            dados.printEncomendas(1, email, 3);
            input.readString();
        }catch (EncomendaInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para avançar");
            input.readString();
        }
    }

    public void encomendasParaEntrega(String email) throws EncomendaInvalidaException{
        try{
            dados.printEncomendas(2,email,1);
            System.out.println("Colocar id encomenda para fazer transporte: ");
            String idEncomenda = input.readString();

            dados.aceitarEncomendaTransporte(email,idEncomenda);
        }catch (EncomendaInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Introduz 0 para avançar");
            input.readString();
        }
    }
    
    public void alterarCusto(String email){
        System.out.println("Custo actual: "+ ((Empresa) dados.getTransportes().get(email)).getCusto());
        System.out.println("Insira novo custo: ");
        float custo = input.readFloat();
        dados.alteraCusto(email,custo);
    }
    
    public void historicoViagens(String email) throws ViagemInvalidaException{
        try{
            dados.printViagens(email);
        }catch (ViagemInvalidaException e){
            System.out.println(e.getMessage());
            System.out.println("Insere 0 para avançar");
            input.readString();
        }
    }

    private  void carregarDados() throws Exception{
        try{
            FileInputStream fis = new FileInputStream("save.csv");
            ObjectInputStream ois = new ObjectInputStream(fis);
            this.dados = (Dados) ois.readObject();
        }catch(IOException | ClassNotFoundException e){
            this.dados = parser.parse();
        }
    }

    private  void guardarDados() throws IOException{
        try{
            FileOutputStream fos = new FileOutputStream("save.csv");
            ObjectOutputStream oos = new ObjectOutputStream(fos);
            oos.writeObject(this.dados);
            oos.close();
        }catch(IOException e){
            e.printStackTrace(System.out);
            System.out.println("Erro ao guardar o ficheiro");
        }
    }
}
