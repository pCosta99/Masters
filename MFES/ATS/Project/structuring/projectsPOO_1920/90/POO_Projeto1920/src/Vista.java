import java.io.Serializable;

/**
 * Classe responsável pela vista
 */
public class Vista implements IVista, Serializable {

    /**
     * MENUS
     */


    public void menuInicial() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------------|");
        System.out.println("|-------------------------------------------------------------------------------------------------|");
        System.out.println("|                                           Traz Aqui!                                            |");
        System.out.println("|                                      Entrega de Encomendas                                      |");
        System.out.println("|-------------------------------------------------------------------------------------------------|");
        System.out.println("|-------------------------------------------------------------------------------------------------|");
    }

    public void menuOpcoes() {
        System.out.println("\n> (1) Criar conta");
        System.out.println("\n> (2) Fazer login");
        System.out.println("\n> (3) Ver os 10 utilizadores mais ativos");
        System.out.println("\n> (4) Ver as 10 empresas transportadoras mais ativas");
        System.out.println("\n> (5) Grava estado do sistema");
        System.out.println("\n> (6) Carregar ficheiro adicional (com algumas entidades criadas)");
        System.out.println("\n> (0) Sair da aplicação");
        System.out.println("\n\n> Opção: ");
    }

    public void showMessage(Object o) {
        System.out.print(o);
    }

    public void clear() {
        System.out.print("\033[H\033[2J");
    }

    public void criarConta() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------------|");
        System.out.println("|                                           Criar conta                                           |");
        System.out.println("|-------------------------------------------------------------------------------------------------|\n");
    }

    public void tipoDeConta() {
        System.out.println("\n(1) Utilizador");
        System.out.println("\n(2) Voluntario");
        System.out.println("\n(3) Loja");
        System.out.println("\n(4) Empresa transportadora");
        System.out.println("\n(0) Voltar");
        System.out.println("\n> Opção: ");
    }

    public void login() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------|");
        System.out.println("|                                           Login                                           |");
        System.out.println("|-------------------------------------------------------------------------------------------|\n");
    }


    public void user() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------|");
        System.out.println("|                                        Utilizador                                         |");
        System.out.println("|-------------------------------------------------------------------------------------------|\n");
    }

    public void loja() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------|");
        System.out.println("|                                          Loja                                             |");
        System.out.println("|-------------------------------------------------------------------------------------------|");
    }

    public void voluntario() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------|");
        System.out.println("|                                        Voluntário                                         |");
        System.out.println("|-------------------------------------------------------------------------------------------|\n");
    }

    public void transportadora() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------|");
        System.out.println("|                                     Transportadora                                          |");
        System.out.println("|-------------------------------------------------------------------------------------------|\n");
    }

    public void funcionalidadesUtilizador() {
        clear();
        user();
        System.out.println("\n(1) Fazer encomenda");
        System.out.println("\n(2) Aceitar serviço transporte");
        System.out.println("\n(3) Histórico de encomendas");
        System.out.println("\n(4) Classificar serviço entrega");
        System.out.println("\n(0) Logout");
        System.out.println("\n> Opção: ");
    }

    public void utilizadorOpcao1SelecionarLoja() {
        clear();
        user();
        System.out.println("\nEscolha a loja: ");
    }

    public void utilizadorOpcao1EncomendaMedica() {
        clear();
        user();
        System.out.println("\nÉ encomenda médica?");
    }

    public void utilizadorOpcao1EscolhaProduto() {
        clear();
        user();
        System.out.println("\nEscolha um produto: ");
    }

    public void utilizadorOpcao1EscolhaDeUnidades() {
        clear();
        user();
        System.out.println("\nQuantas unidades deste produto quer comprar? > ");
    }

    public void utilizadorOpcao1ConfirmarEnc() {
        clear();
        user();
        System.out.println("\nConfirmar encomenda?");
    }

    public void funcionalidadesLojaComInfoEspera() {
        clear();
        loja();
        System.out.println("\n(1) Gerir encomendas");
        System.out.println("\n(2) Gerir fila de espera");
        System.out.println("\n(3) Histórico de encomendas");
        System.out.println("\n(0) Logout");
        System.out.println("\n> Opção: ");
    }

    public void funcionalidadesLojaSemInfoEspera() {
        clear();
        loja();
        System.out.println("\n(1) Gerir encomendas");
        System.out.println("\n(2) Histórico de encomendas");
        System.out.println("\n(0) Logout");
        System.out.println("\n> Opção: ");
    }


    public void funcionalidadesMeioTransporte(boolean isVoluntario) {
        clear();
        if (isVoluntario)
            voluntario();
        else
            transportadora();

        System.out.println("\n(1) Gerir disponibilidade");
        System.out.println("\n(2) Escolher encomenda a entregar");
        System.out.println("\n(3) Finalizar encomenda");
        System.out.println("\n(4) Histórico de encomendas");
        if (!isVoluntario) System.out.println("\n(5) Total faturado entre datas");
        System.out.println("\n(0) Logout");
        System.out.println("\n> Opção: ");
    }


    public void top10() {
        clear();
        System.out.println("\n|-------------------------------------------------------------------------------------------|");
        System.out.println("|                                           TOP 10                                          |");
        System.out.println("|-------------------------------------------------------------------------------------------|\n");

    }
}
