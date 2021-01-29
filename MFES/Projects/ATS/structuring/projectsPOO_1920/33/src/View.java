import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class View {

    /**
     * Imprime o menu Inicial
     */
    public void loginMenu(){
        System.out.println("################################### TRAZAQUI ###################################");
        System.out.println("##                                                                            ##");
        System.out.println("## 1 -> Login                                                                 ##");
        System.out.println("## 2 -> Registo                                                               ##");
        System.out.println("## 3 -> Carregar do parser                                                    ##");
        System.out.println("## 4 -> Guardar estado                                                        ##");
        System.out.println("## 5 -> Carregar estado                                                       ##");
        System.out.println("## 0 -> Sair sem guardar                                                      ##");
        System.out.println("##                                                                            ##");
        System.out.println("################################################################################");
        System.out.print("  Opção: ");




     //   System.out.println(("10 : ver serviço todo, so para testes"));
    }



    /**
     * Imprime os atores do sistema
     */
    public void menuAtores(){
        System.out.println("utilizador ");
        System.out.println("voluntario  ");
        System.out.println("loja ");
        System.out.println("transportadora  ");
        System.out.println("Insira 0 pra sair!  ");
    }



    /**
     * Imprime o menu Utilizador
     */
    public void menuUtilizador(){
        System.out.println("1 : Fazer encomenda \n");
        System.out.println("2 : Classificar entrega \n");
        System.out.println("0 : logout\n ");

    }

    /**
     * Imprime o menu voluntario
     */
    public void menuVoluntario(){
        System.out.println("1 : Fazer encomenda \n");
        System.out.println("0 : logout\n ");
    }

    /**
     * Imprime o menu transportadora
     */
    public void menuTransportadora(){
        System.out.println("1 : Fazer encomenda \n");
        System.out.println("0 : logout\n ");
    }

    /**
     * Imprime o menu Utilizador
     */
    public void menuLoja(){
        System.out.println("1 : Sinalizar encomenda \n");
        System.out.println("0 : logout\n ");
    }



    /**
     * escolhe a loja que vai fazer a encomenda
     */
    public String[] lerProdutos( String produtos){
        System.out.println("Esolha os produto que pretenta escolher, imprima 0 para sair!!");
        System.out.println(produtos);
        String produto[] = new String[2];
        System.out.println("Escolha o codigo do produto que deseja");
        produto[0] = lerString();
        System.out.println("Escolha a quantidade");
        produto[1] = lerString();
       return produto;
    }


    /**
     * escolhe a transportadora que vai classificar
     */
    public String[] classificaEntrega(){
        System.out.println("escolhe a transportadora que vai classificar, imprima 0 para sair!!");
        String produto[] = new String[2];
        System.out.println("Escolha o codigo  que deseja");
        produto[0] = lerString();
        System.out.println("Escolha a classificacao");
        produto[1] = lerString();
        return produto;
    }




    /**
     * escolhe os produtos da loja que vai fazer a encomenda
     */
    public String lerLoja( String lojas){
        System.out.println("Esolha o codigo da loja em que quer fazer a encomenda");
        System.out.println(lojas);
        String codLoja = lerString();
        return codLoja;
    }

    /**
     * escolhe os produtos da loja que vai fazer a encomenda
     */
    public String lerEncomenda( String encomedas){
        System.out.println("Esolha o codigo da encomenda em que que prende alterar");
        System.out.println(encomedas);
        String codLoja = lerString();
        return codLoja;
    }

    /**
     * escolhe os produtos que podem ser transportados
     *
     */
    public String lerEncomendaTransporte( String encomedas){
        System.out.println("Esolha o codigo do transporte que prende efetuar");
        System.out.println(encomedas);
        String codenc = lerString();
        return codenc;
    }



    /**
     * Login de login
     * @return Strings dos campos do login
     */
    public String[] login(){
        String[] emPass = new String[2];
        System.out.println("Insira o email");
        emPass[0] = lerString();
        System.out.println("Insira a Password");
        emPass[1] = lerString();
        return emPass;
    }

    /**
     * Registo
     * @return Strings dos campos do login, Tipo de ator, email, password
     */
    public String[] registo(){

        String[] atorEmPass = new String[9];
        System.out.println("Insira tipo de ator no sistema\n");
        menuAtores();
        atorEmPass[0] = lerString();
        switch (atorEmPass[0]) {
            case "utilizador":
            case "loja":
                System.out.println("Insira o email");
                atorEmPass[1] = lerString();
                System.out.println("Insira a Password");
                atorEmPass[2] = lerString();
                System.out.println("Insira o nome");
                atorEmPass[3] = lerString();
                System.out.println("Insira a latitude");
                atorEmPass[4] = lerString();
                System.out.println("Insira a longitude");
                atorEmPass[5] = lerString();
                break;
            case "voluntario":

                System.out.println("Insira o email");
                atorEmPass[1] = lerString();
                System.out.println("Insira a Password");
                atorEmPass[2] = lerString();
                System.out.println("Insira o nome");
                atorEmPass[3] = lerString();
                System.out.println("Insira a latitude");
                atorEmPass[4] = lerString();
                System.out.println("Insira a longitude");
                atorEmPass[5] = lerString();
                System.out.println("Insira raio de trabalho");
                atorEmPass[6] = lerString();

                break;

            case "transportadora":

                System.out.println("Insira o email");
                atorEmPass[1] = lerString();
                System.out.println("Insira a Password");
                atorEmPass[2] = lerString();
                System.out.println("Insira o nome");
                atorEmPass[3] = lerString();
                System.out.println("Insira a latitude");
                atorEmPass[4] = lerString();
                System.out.println("Insira a longitude");
                atorEmPass[5] = lerString();
                System.out.println("Insira raio de trabalho");
                atorEmPass[6] = lerString();
                System.out.println("Insira preço por km");
                atorEmPass[7] = lerString();
                System.out.println("Insira o nif");
                atorEmPass[8] = lerString();

                break;

        }
        return atorEmPass;
    }



    /**
     * Método para ler strings
     * @return String lida
     */
    public String lerString() {
        Scanner input = new Scanner(System.in);
        boolean flag = false;
        String txt = "";
        while(!flag) {
            try {
                txt = input.nextLine();
                flag = true;
            } catch(InputMismatchException e) {
                System.out.println("Formato inválido");
                System.out.print("Nova opção: ");
                input.nextLine();
            }
        }
        return txt;
    }

    /**
     * Metodo para ler inteiro
     * @return Inteiro lido
     */
    public int lerInt() {
        Scanner input = new Scanner(System.in);
        boolean flag = false;
        int i = 0;
        while(!flag) {
            try {
                i = input.nextInt();
                flag = true;
            } catch(InputMismatchException e) {
                System.out.println("Inteiro Invalido");
                System.out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return i;
    }
}


