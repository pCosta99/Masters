import java.util.Scanner;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Menu {
    
    public void janela_inicial() {
        cls();
        System.out.println("\n\t\tBem-Vindo a  TrazAqui!\n");
        System.out.println("\t Escolha uma das seguintes opcoes:\n");
        System.out.println("\t\t1 -> Fazer Login");
        System.out.println("\t\t2 -> Registar Novo Utilizador");
        System.out.println("\t\t3 -> Sair");
        System.out.print("\n\tOpcao: ");
    }
    
    public void escolhaFuncao(){
        System.out.println("\t\tQual a sua funcao:");
        System.out.println("\t\t1 -> Voluntario");
        System.out.println("\t\t2 -> Transportadora");
        System.out.println("\t\t3 -> Loja");
        System.out.println("\t\t4 -> Cliente");
        System.out.println("\n\n\tOpcao: ");
    }
    
    public void registoComSucesso(String userName) {
        System.out.print("\f");
        System.out.println("\n\t**A tua conta encontra-se agora ativa**\n");
        System.out.println("\n\t**Podes agora fazer login, " + userName + "!**\n\n");
        pressionaUmaTecla();
    }
    
    public void janelaPrincipalAdmin(){
        cls();
        System.out.println("\n\n-----------------ENTROU NO MENU Administrador------------------\n\n");
        System.out.println("\t\tEscolha uma das seguintes opcoes:\n");
        System.out.println("\t\t1 -> Top 10 Clientes com mais encomendas");
        System.out.println("\t\t2 -> Top 10 Transportadoras por kms");
        System.out.println("\t\t3 -> Sair");
        System.out.print("\n\tOpcao: ");
    
    }

    public void janelaPrincipalVoluntario() {
        cls();
        System.out.println("\n\n-----------------ENTROU NO MENU Voluntario------------------\n\n");
        System.out.println("\t\tEscolha uma das seguintes opcoes:\n");
        System.out.println("\t\t1 -> Alterar Disponibilidade");
        System.out.println("\t\t2 -> Escolher Entrega");
        System.out.println("\t\t3 -> Fazer Transporte e tempo demorado");
        System.out.println("\t\t4 -> Sair");
        System.out.print("\n\tOpcao: ");

    }

    public void janelaPrincipalLoja() {
        cls();
        System.out.println("\n\n---------------------ENTROU NO MENU Loja----------------------\n\n");
        System.out.println("\t\tEscolha uma das seguintes opcoes:\n");
        System.out.println("\t\t1 -> Ver artigos");
        System.out.println("\t\t2 -> Quantidade de pessoas em espera");
        System.out.println("\t\t3 -> Validar pedidos");
        System.out.println("\t\t4 -> Sair");
        System.out.print("\n\tOpcao: ");
    }

    public void janelaPrincipalTransportadora() {
        cls();
        System.out.println("\n\n-------------------ENTROU NO MENU Transportadora----------------\n\n");
        System.out.println("\t\tEscolha uma das seguintes opcoes:\n");
        System.out.println("\t\t1 -> Alterar Disponibilidade");
        System.out.println("\t\t2 -> Escolher Entrega");
        System.out.println("\t\t3 -> Total faturado entre datas");
        System.out.println("\t\t4 -> Consultar avaliacao");
        System.out.println("\t\t5 -> Sair");
        System.out.print("\n\tOpcao: ");
    }

    public void janelaPrincipalCliente() {
        cls();
        System.out.println("\n\n--------------------------ENTROU NO MENU Cliente---------------------\n\n");
        System.out.println("\t\tEscolha uma das seguintes opcoes:\n");
        System.out.println("\t\t1 -> Solicitar Entrega");
        System.out.println("\t\t2 -> Aceitar ou Nao Transporte");
        System.out.println("\t\t3 -> Consultar info de entrega(Entre Datas)");
        System.out.println("\t\t4 -> Classificar Transporte");
        System.out.println("\t\t5 -> Ver lista de notificacoes");
        System.out.println("\t\t6 -> Sair");
        System.out.print("\n\tOpcao: ");
    }

    public void pausar() {
        System.out.println("\tPressione qualquer tecla para continuar\n");
        Scanner keyboard = new Scanner(System.in);
        keyboard.nextLine();
        keyboard.close();
    }
    
    public void pressionaUmaTecla() {
        System.out.println("\n\tPressione ENTER para Voltar...");
        try {
            System.in.read();
        } catch (Exception e) {
        }
    }

    public void cls() {
        System.out.print("\f");
    }

    public void janela_registo() {
        cls();
        System.out.println("\n\t\tRegistar novo Utilizador.\n");

    }

    public void janela_produto() {
        cls();
        System.out.println("\n\t\tRegistar novo produto ou acrescentar.\n");

    }

    public void janela_login() {
        cls();
        System.out.print("\n\t\tMenu Login\n");
        System.out.print("\n\tInsira o seu Email e Password:\n");

    }

    public void janela_Cliente() {
        cls();
        System.out.println("\n\t\tRegistar novo Cliente.\n");

    }

    public void janela_Loja() {
        cls();
        System.out.println("\n\t\tRegistar nova Loja.\n");

    }

    public void janela_Voluntario() {
        cls();
        System.out.println("\n\t\tRegistar novo Voluntario.\n");

    }

    public void janela_Transportadora() {
        cls();
        System.out.println("\n\t\tRegistar nova Transportadora.\n");

    }

    public void sair() {
        cls();
        System.out.println("\n\n\t\tObrigado pela visita!\n");

    }

    public void disponibilidade() {
        System.out.println("\t\t1 -> Disponivel");
        System.out.println("\t\t2 -> Indisponivel");
        System.out.println("\t\t3 -> Voltar");
        System.out.println("\tInsira a Opcao: ");
    }

    public void aceitatransporte(List<Viagem> viagens) {
        cls();
        System.out.println("Qual a transportadora que quer avaliar?");
        System.out.println("\n\tCodigo | Nome");
        for(Viagem v : viagens){
            System.out.println("\t" + v.getCodTransportadora() + " | " + v.getNomeTransportadora());
        }
        System.out.println("\tInsira o codigo da transportadora: ");
    }

    public void menu_viagem() {
        cls();
        System.out.println("\t\tFazer Viagem\n");
        System.out.println("\t\t1 -> Viatura mais prÃ³xima");
        System.out.println("\t\t2 -> Motoristas");
        System.out.println("\t\t3 -> Voltar");
        System.out.println("\tInsira a Opcao: ");
    }

    public void limparNotificacoes() {
        System.out.println("Deseja limpar as suas notificacoes? [s/n]");
    }

    public void simOuNao() {
        System.out.println(Constantes.SIM);
        System.out.println(Constantes.NAO);
    }
    
    public void listaArtigos(Map<String,LinhaEncomenda> produtos){
        cls();
        System.out.println("\t\tLista de produtos");
        System.out.println("\n\tcod | descricao |quantidade| valor");
        for(LinhaEncomenda le : produtos.values()){
            System.out.println("\t" + le.getCodProduto() + " | " + le.getDescricao() + " | " + le.getQuantidade() + "kg | " + le.getValorUnitario() + "€");
        }
        
    }
    
    public void listaLojas(Map<String,Loja> lojas){
        cls();
        System.out.println("\t\tLista de Lojas");
        System.out.println("\n\tcod | descricao | posX | posY");
        for(Map.Entry<String,Loja> entry : lojas.entrySet()){
            System.out.println("\t"+ entry.getKey() + " | " + entry.getValue().getNome() + " | " + entry.getValue().getPosX() + " | " + entry.getValue().getPosY());
        }
    }
    
    public void editarArtigos(){
        System.out.println("\n\n\t1 -> Alterar/acrescentar artigo");
        System.out.println("\t2 -> Sair");    
    }
    
    public void top10Transportadoras(List<Transportadora> transportadoras){
        cls();
        System.out.println("\t\tAs 10 transportadoras com mais kmks:\n");
        System.out.println("\tNr.  | codigo | nome | totalKms ");
        int i = 1;
        for(Transportadora t : transportadoras){
            System.out.println("\t" + i + "  |  " + t.getCodEmpresa() + "  | " + t.getNome() + " | " + t.getTotalKms());
        }
        pressionaUmaTecla();
    }
    
    public void top10Clientes(List<ClienteViagem> clientes){
        cls();
        System.out.println("\t\tAs 10 clientes com mais viagens:\n");
        System.out.println("\tNr.  | codigo | nome | numero");
        int i = 1;
        for(ClienteViagem t : clientes){
            
            System.out.println("\t" + i + "  |  " + t.getCliente().getCodCliente() + "  | " + t.getCliente().getNome() + " | " + t.getNrViagens());
        }
        pressionaUmaTecla();
    }
    public void listaCodEncomendas(Map<String,Encomenda> aceites) {
        for (String s : aceites.keySet()) {
            System.out.println("Codigo encomenda: " + s);
        }
    }

    public void escolhaViagens(List<Viagem> viagens){
        cls();
        System.out.println("\t\tEscolha a viagem que pretende:\n");
        System.out.println("\tNr. | Transportadora | custo");
        int i = 1;
        for(Viagem v : viagens){
            System.out.println("\t" + i + " | " + v.getNomeTransportadora() + " | " + v.getCusto());
            i++;
        }
    }
    
    public void listaCodEncomendas(Set<String> cods){
        cls();
        System.out.println("\t\tInsira o código da encomenda que pretende:\n");
        int i = 1;
        for(String c : cods){
            System.out.println(i + " -> " + c);
            i++;
        }
    }
    
    public void listaViagens(List<Viagem> viagens){
        cls();
        System.out.println("\tCodTrans | Transportadora | custo");
        
        for(Viagem v : viagens){
            System.out.println("\t" + v.getCodTransportadora() + " | " + v.getNomeTransportadora() + " | " + v.getCusto());
            
        }
    }
    
    public void mostrarAvaliacao(double avaliacao){
        cls();
        System.out.println("A sua avaliacao é " + avaliacao);
    }
}