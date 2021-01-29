package View;

import Model.Encomenda;

import java.util.List;
import java.util.Map;

/** Class responsible for presenting information to the user.
 *
 */
public class Output {
    public void startingMenu(){
        System.out.println("Bem vindo ao TrazAqui!");
        System.out.println("1.- Utilizador");
        System.out.println("2.- Loja");
        System.out.println("3.- Voluntário");
        System.out.println("4.- Transportadora");
        System.out.println("5.- Administrador");
        System.out.println("6.- Sair");
    }

    public void loginMenu(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Login");
        System.out.println("2.- Registar");
        System.out.println("3.- Anterior");
    }

    // User
    public void userMenu(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Fazer uma nova encomenda");
        System.out.println("2.- Pedir entrega de uma encomenda");
        System.out.println("3.- Dar Encomenda como recebia");
        System.out.println("4.- Histórico de compras");
        System.out.println("5.- Definicoes da conta");
        System.out.println("6.- Sair");
    }
    public void userEdit(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Alterar o nome de utilizador");
        System.out.println("2.- Alterar localizaçao");
        System.out.println("3.- Alterar password");
        System.out.println("4.- Eliminar conta");
        System.out.println("5.- Sair");
    }
    public void receberNomeLoja(){
        System.out.println("Insira o nome da loja premitida");
    }
    public void invalidLoja(){System.out.println("Loja invalida");}
    public void productDisplay(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Escolher um produto");
        System.out.println("2.- Proxima pagina");
        System.out.println("3.- Pagina anterior");
        System.out.println("4.- Sair");
    }
    public void getProdName(){System.out.println("Insira o numero do produto");}
    public void getQuantity(){System.out.println("Insira a quantidade que deseja deste produto");}
    public void userName(){
        System.out.println("Insira o seu nome.");
    }
    public void voluntariosDisponiveis(int s){System.out.println("Foram encontrados "+ s +" voluntários disponíveis.");}
    public void empresasDisponiveis(int s){System.out.println("Foram encontradas "+s+" empresas transportadoras disponíveis.");}
    public void noVolnorEmpFound(){
        System.out.println("Não foram encontrados voluntários nem Empresas Transportadoras disponíveis para esta entrega. Tente novamente mais tarde.");
    }
    public void perguntaVolOuET(){
        System.out.println("Deseja ver a lista dos Voluntários(1) ou das Empresas Transportadoras(2) disponíveis para esta entrega?");
    }
    public void isMedical(){
        System.out.println("Esta encomenda é medicinal?");
    }//help my pt plz

    //Store
    public void storeMenu(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Editar o catalogo de produtos");
        System.out.println("2.- Confirmar encomendas"); // este pode por a frente quantas faltam
        System.out.println("3.- Histórico de Vendas");
        System.out.println("4.- Definicoes da conta");
        System.out.println("5.- Sair");
    }
    public void storeEdit(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Alterar o nome da Loja");
        System.out.println("2.- Alterar a localização da loja");
        System.out.println("3.- Alterar password");
        System.out.println("4.- Eliminar conta");
        System.out.println("5.- Sair");
    }
    public void productStoreMenu(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Adicionar um produto ao catalogo");
        System.out.println("2.- Remover um produto do catalogo"); // este pode por a frente quantas faltam
        System.out.println("3.- Editar um produto");
        System.out.println("4.- Proxima pagina");
        System.out.println("5.- Pagina anterior");
        System.out.println("6.- Sair");
    }
    public void registerStoreName(){
        System.out.println("Insira o nome da sua Loja:");
    }
    public void productEdit(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Editar a referencia");
        System.out.println("2.- Editar a descricao");
        System.out.println("3.- Editar preco");
        System.out.println("4.- Sair");
    }
    public void invalidProduct(){
        System.out.println("Numero de produto invalido");
    }
    public void encomendaDisplay(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Aceitar uma encomenda");
        System.out.println("2.- Proxima pagina");
        System.out.println("3.- Pagina anterior");
        System.out.println("4.- Sair");
    }
    public void volDisplay(){
        System.out.println("Insira a opção que pretende executar");
        System.out.println("1.- Escolher um voluntário");
        System.out.println("2.- Proxima pagina");
        System.out.println("3.- Pagina anterior");
        System.out.println("4.- Sair");
    }
    public void invalidEncomenda(){
        System.out.println("Escolha inválida. Tente novamente.");
    }
    public void invalidVoluntario(){
        System.out.println("Escolha inválida. Tente novamente.");
    }
    public void getEncName(){System.out.println("Insira o numero da encomenda");}
    public void getVolName(){System.out.println("Insira o numero do Voluntário");}
    public void faltamXEnc(int s){
        System.out.println("Tem de momento "+ s + " encomendas por aceitar.");
    }
    public void printHistEnc(String encomenda){
            System.out.println(encomenda);
    }
    //addProduct
    public void addReferencia(){
        System.out.println("Insira a referencia");
    }
    public void addDescricao(){
        System.out.println("Insira a descricao");
    }
    public void addPreco(){
        System.out.println("Insira o preco");
    }
    public void productSuccessful(){
        System.out.println("Produto adicionado com sucesso");
    }
    //removeProduct
    public void getRemove(){
        System.out.println("Insira o numero do produto a remover");
    }
    public void removeSuccessful(){
        System.out.println("Produto eleminado com sucesso");
    }
    //editProduct
    public void getEdit(){
        System.out.println("Insira o numero do produto a editar");
    }

    //Volunteer
    public void volunteerMenu(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Encomendas pendentes");
        System.out.println("2.- Definicoes da conta");
        System.out.println("3.- Historico de entregas");
        System.out.println("4.- Sair");
    }
    public void volunteerEdit(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Alterar o nome");
        System.out.println("2.- Alterar a localizacao");
        System.out.println("3.- Alterar password");
        System.out.println("4.- Eliminar conta");
        System.out.println("5.- Sair");
    }
    public void volunteerMessage(){
        System.out.println("Obrigado pela sua contribuicao");
    }

    //Trasportadora
    public void transMenu(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Ver Encomendas pendentes");
        System.out.println("2.- Definicoes da conta");
        System.out.println("3.- Ver Total faturado");
        System.out.println("4.- Ver Historico de entregas");
        System.out.println("5.- Sair");
    }
    public void transEdit(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Alterar o nome");
        System.out.println("2.- Alterar a localizacao");
        System.out.println("3.- Alterar password");
        System.out.println("4.- Eliminar conta");
        System.out.println("5.- Sair");
    }
    public void transName(){
        System.out.println("Insira o nome da sua empresa transportadora.");
    }
    public void registerTransName(){
        System.out.println("Insira o nome da sua Empresa:");
    }
    public void registerNif(){
        System.out.println("Insira o seu NIF:");
    }
    public void registerTaxaPeso(){
        System.out.println("Insira o sua taxa por Kg:");
    }
    public void registerTaxaKm(){
        System.out.println("Insira o sua taka por Km:");
    }
    public void totalFaturado(){
        System.out.println("Insira o periodo de tempo que pretende ver os seus rendimentos");
        System.out.println("1.- Uma Semana");
        System.out.println("2.- Um mês");
        System.out.println("3.- Desde o inicio"); //portugeus mal dizido
        System.out.println("4.- Sair");
    }
    public void printMoney(Double money){
        System.out.println("A empresa recebeu " + money + "€");
    }

    //Administrador
    public void admMenu(){
        System.out.println("Insira a opção que pretende executar.");
        System.out.println("1.- Inserir ficheiro de logs");
        System.out.println("2.- Visualizar utilizadores");
        System.out.println("3.- Estatisticas");
        System.out.println("4.- Eleminar o estado da aplicacao");
        System.out.println("5.- Visualizar encomendas");
        System.out.println("6.- Sair");
    }
    public void loginType(){
        System.out.println("Insira a lista de registos que pretende ver");
        System.out.println("1.- Utilizador");
        System.out.println("2.- Loja");
        System.out.println("3.- Voluntário");
        System.out.println("4.- Transportadora");
        System.out.println("5.- Administrador");
        System.out.println("6.- Sair");
    }
    public void manageAccounts(){
        System.out.println("Insira a lista de registos que pretende ver");
        System.out.println("1.- Eleminar uma conta");
        System.out.println("2.- Sair");
    }
    public void deleteAccount(){
        System.out.println("Insira o email da conta que pretende remover.");
    }
    public void nuke(){
        System.out.println("Tem a certeza que pretende eleminar o estado atual? \nToda a informacao sera perdida.");
        System.out.println("1.- Sim, tenho a certeza");
        System.out.println("2.- Retroceder");
    }
    public void manageEncomendas(){
        System.out.println("Insira a lista de registos que pretende ver");
        System.out.println("1.- Eleminar uma encomenda");
        System.out.println("2.- Sair");
    }
    public void deleteEncomenda(){
        System.out.println("Insira o codigo da encomenda que pretende remover.");
    }
    public void removeFailed(){
        System.out.println("Codigo de encomenda invalido");
    }

    //Location
    public void registerLocation(){
        System.out.println("Insira a sua localizaçao:");
    }
    public void registerStoreLocation(){System.out.println("Insira a localização da loja:");}
    public void locationLongitude(){System.out.println("x = ");}
    public void locationLatitude(){System.out.println("y = ");}

    //Login
    public void email(){
        System.out.println("Insira o seu email");
    }
    public void loginSuccessful(){
        System.out.println("Login successful.");
    }
    public void loginFailed(){
        System.out.println("Login failed, please try again.");
    }
    public void password(){
        System.out.println("Insira a sua password.");
    }
    public void repeatPassword(){
        System.out.println("Confirme a sua password.");
    }
    public void registerSuccessful(){
        System.out.println("Register successful.");
    }
    public void registerFailed(){
        System.out.println("Register failed, please try again.");
    }
    public void oldPassword(){
        System.out.println("Insira a sua password atual.");
    }
    public void newPassword(){
        System.out.println("Insira a sua nova password.");
    }
    public void passwordRepeat(){System.out.println("As passwords não coicidem. Tente novamente!");}
    public void changePasswordSuccessful(){
        System.out.println("Alteração da sua password bem sucedida.");
    }
    public void changePasswordFailed(){
        System.out.println("Ocorreu um erro na alteracao da sua password.");
    }


    public void welcomeUser(String name){
        System.out.println("Bem vindo " + name + "!");
    }
    public void limiteException(){
        System.out.println("Limite do catalogo excedido");
    }
    public void registerStore(){
        System.out.println("Bem vindo ao TrazAqui!");
    }


    public void exit(){
        System.out.println("Terminando.");
    }
    public void invalidCommand(){
        System.out.println("Comando inválido, tente novamente.");
    }
    public void printString(String linha){
        System.out.println(linha);
    }
    public void perguntaFile(){System.out.print("Insira o nome do ficheiro que pretende carregar (nome+'.txt'): ");}
    public void getOutKey(){System.out.println("Qualquer tecla + Enter para sair!");}

    public void loading(){
        System.out.println("Loading...");
    }
    public void loadingCompleted(){
        System.out.println("Loading completed!");
    }

    public void confirmarRemocaoConta(){
        System.out.println("Tem a certeza que pretende remover a sua conta?");
    }

    public void toptenMenu(){
        System.out.println("Insira o top 10 que pretende visualiar");
        System.out.println("1.- Top 10 utilizadores com mais encomendas");
        System.out.println("2.- Top 10 Transportadores com maior distancia percorrida");
        System.out.println("3.- Sair");
    }
    public void tenUser(){
        System.out.println("Top 10 utilizadores com mais encomendas");
    }
    public void tenTrans(){
        System.out.println("Top 10 Transportadoras com maior distancia percorrida");
    }
    public void printList(List<String> list){
        for(int i = 0; i<list.size();i++){
            System.out.println( i+1 + ": " + list.get(i));
        }
    }
    public void printMap(Map<String,Integer> map){
        int i = 1;
        for(Map.Entry<String, Integer> entry : map.entrySet()){
            System.out.println( i++ + ": " + entry.getKey() + " = " + entry.getValue() + " encomendas.");
        }
    }
    public void printMapDouble(Map<String,Double> map){
        int i = 1;
        for(Map.Entry<String, Double> entry : map.entrySet()){
            System.out.println( i++ + ": " + entry.getKey() + " = " + entry.getValue() + " percorridos.");
        }
    }

    public void perguntaRate(){
        System.out.println("Classifique o transportador! (0-5)");
    }
}
