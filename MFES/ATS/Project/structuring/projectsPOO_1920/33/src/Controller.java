import java.io.IOException;
import java.io.Serializable;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.Scanner;

public class Controller {
    private Service servico;
    private View view;

    /**
     * Metodo main que cria a aplicação e chama o run()
     * @param args
     * @throws IOException
     */
    public static void main(String[] args) throws IOException, ClassNotFoundException, AtorInvalidoException {
        new Controller().run();
    }

    private Controller(){
        this.servico = new Service();
        this.view = new View();
    }

    /**
     * Metodo que trata do menu inicial
     */
    private void run() throws IOException, ClassNotFoundException, AtorInvalidoException {
        int escolha = -1;
        do {
            view.loginMenu();
            escolha = view.lerInt();
            switch (escolha){
                case 0:
                    System.out.println("Obrigado!");
                    break;
                case 1:
                    String ator;

                    String[] credenciais = view.login();
                    ator = servico.login(credenciais);
                    if(ator.equals("Credenciais Erradas")) {
                        System.out.println("Credenciais Erradas tente de novo!!");
                        break;
                    }
                    runLogin(ator,credenciais[0]);
                    break;
                case 2:
                    int flag =  -1;
                    do {
                        String[] registo = view.registo();

                        try {
                            flag = servico.registo(registo);
                            if(flag!=0){ System.out.println(registo[0] + " criado com sucesso!");
                            flag = 0;}
                        } catch (PessoaRepetidaException | EmpresaRepetidaException | AtorInvalidoException | NumberFormatException e) {
                            System.out.println(e.getMessage());

                        }
                    }while (flag!=0);
                    break;

                case 3 :
                    Parse p = new Parse();

                    p.parse("src/logs.csv",servico);
                    break;

                case 4:
                    servico.guardaEstado("Service");
                    System.out.println("Gravado");
                    break;
                case 5:
                    servico = servico.carregaEstado("Service");
                    System.out.println("Serviço carregado com sucesso!");
                    break;

                case 10 :
                //    servico.classificarEntrega("t9",10);
                 //   System.out.println(servico.getTransportadoras().get("t9").getclassificacoes());

                    //   System.out.println(servico.melhoresUtilizadores());
               //     System.out.println(servico.transportadorasMaisKms());
                   System.out.println(servico.toString());
                    break;

                default:
                    System.out.println("Opçao inexistente!!\n");
                    break;
            }
        }while(escolha!=0);
    }

    /**
     * Metodo que trata do menu inicial de utilizador
     */
    private void runLogin(String ator,String email) {
        int escolha = -1;

            switch (ator) {
                case "utilizador":
                    runUtilizador(servico.getUtilizadorEmail(email));
                    break;
                case "loja":
                    runLoja(servico.getLojaEmail(email));
                    break;
                case "voluntario":
                    runVoluntario(servico.getVoluntarioEmail(email));
                    break;
                case "transportadora":
                    runTransportadora(servico.getTransportadoraEmail(email));
                    break;
            }



    }

    /**
     * Metodo que trata do menu inicial
     */
    private void runUtilizador(String codUtilizador) {
        int escolha = -1;
        do {
            view.menuUtilizador();
            escolha = view.lerInt();
            switch (escolha) {
                case 0:
                    break;
                case 1:
                    String codLoja=view.lerLoja(servico.getLojasCodNome());
                    LinhaEncomenda le = new LinhaEncomenda();
                    int continu ;
                    do {
                        try {
                            String prods = servico.getProdutosLoja(codLoja);
                            String[] dados = view.lerProdutos(prods);
                            Produto prod = servico.atualizaProdutos(dados[0], Double.parseDouble(dados[1]), codLoja);
                            le.addProduto(prod);

                        } catch (LojaInexistenteException | QuantidadeEmExcessoException | ProdutoInexistenteException e) {
                            System.out.println(e.getMessage());
                        }
                        System.out.println("Imprima 0 nao quiser mais produtos!, caso contraio noutro nr");
                        continu = view.lerInt();
                    }while (continu != 0);
                    servico.addEncomenda(codUtilizador,codLoja,le);



                    break;
                case 2:
                    String[] argss = view.classificaEntrega();
                    try {
                        servico.classificarEntrega(argss[0], Integer.parseInt(argss[1]));
                    }catch (AtorInvalidoException e){
                        System.out.println(e.getMessage());
                    }
            }
        }while(escolha!=0);

    }

    private void runLoja(String codLoja){
        int escolha = -1;
        do {
            view.menuLoja();
            escolha = view.lerInt();
            switch (escolha) {
                case 0:
                    break;
                case 1:
                    String codEncomenda = view.lerEncomenda(servico.getEncomendasLoja(codLoja));
                    servico.alteraEstado(codEncomenda,codLoja);
                    break;
            }
        }while(escolha!=0);

    }

    private void runVoluntario(String codVol){
        int escolha = -1;
        do {
            view.menuVoluntario();
            escolha = view.lerInt();
            switch (escolha) {
                case 0:
                    break;
                case 1:
                    String codEncomenda = view.lerEncomenda(servico.getEncomendasParaTransporteVol(codVol));
                    servico.alteraEstadoVoluntario(codEncomenda,codVol);
                    break;

            }
        }while(escolha!=0);

    }

    private void runTransportadora(String codVol){
        int escolha = -1;
        do {
            view.menuTransportadora();
            escolha = view.lerInt();
            switch (escolha) {
                case 0:
                    break;
                case 1:
                    String codEncomenda = view.lerEncomendaTransporte(servico.getEncomendasParaTransporte(codVol));
                    servico.alteraEstadoTransportadora(codEncomenda,codVol);
                    break;
            }
        }while(escolha!=0);

    }





}
