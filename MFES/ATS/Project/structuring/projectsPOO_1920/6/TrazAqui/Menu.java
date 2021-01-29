
/**
 * Classe dos Menus
 * 
 * @author (João Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
public class Menu
{
    public static void menuPrincipal(){
        System.out.println(
                "1 - Entrar entidade\n"+
                "2 - Registar entidade\n"+
                "\n0 - Sair");
    }
    public static void menuEntrar(){
        System.out.println(
                "1 - Entrar utilizador\n"+
                "2 - Entrar voluntario\n"+
                "3 - Entrar empresa transportadora\n"+
                "4 - Entrar loja\n"+
                "5 - Entrar Administrador\n"+
                "\n0 - Voltar");
    }
    public static void menuOpcoesUtilizador(){
        System.out.println(
                "1 - Inserir pedido de encomenda\n"+
                "2 - Classificar servico de encomenda\n"+
                "3 - Lista de Encomendas Entregues por Voluntario\n"+
                "4 - Lista de Encomendas Entregues por Transportadora\n"+
                "5 - Encomendas do Utilizador\n"+
                "6 - Aceitar encomenda\n"+
                "\n0 - Voltar");
    }
    public static void menuOpcoesVoluntario(){
        System.out.println(
                "1 - Informacao encomendas transportadas por data de transporte\n"+
                "2 - Colocar como disponivel\n"+
                "\n0 - Voltar");
    }
    public static void menuOpcoesEmpresaTransportadora(){
        System.out.println(
                "1 - Informacao encomendas transportadas por data de transporte\n"+        
                "2 - Total facturado num determinado periodo\n"+
                "3 - Colocar como disponivel\n"+
                "\n0 - Voltar");
    }
    public static void menuOpcoesLoja(){
        System.out.println(
                "1 - Lista de Encomendas Prontas\n"+
                "2 - Associar encomenda pronta\n"+
                "\n0 - Voltar");
    }
    public static void menuOpcoesAdministrador(){
        System.out.println(
                "1 - Lista das 10 empresas transportadoras que mais usam a aplicacao\n"+
                "2 - Lista dos 10 utilizadores que mais usam a aplicacao\n"+
                "3 - Classificacao de Encomenda\n"+
                "4 - Lista Utilizadores\n"+
                "5 - Lista Lojas\n"+
                "6 - Lista Voluntarios\n"+
                "7 - Lista Empresas Transportadoras\n"+
                "8 - Lista Encomendas\n"+
                "9 - Lista Encomendas Prontas\n"+
                "10 - Lista Encomendas Aceites\n"+
                "11 - Lista Encomendas Transportadora\n"+
                "12 - Lista Encomendas Voluntario\n"+
                "13 - Lista Voluntarios Disponiveis\n"+
                "14 - Lista Transportadoras Disponiveis\n"+
                "15 - Lista Transportadoras Indisponiveis\n"+
                "16 - Data Encomendas Entregues\n"+
                "17 - Duracao Encomendas Entregues\n"+
                "18 - Encomendas de Utilizador por Aceitar\n"+
                "\n0 - Voltar");
    }
    public static void menuRegistarEntidade(){
        System.out.println(
                "1 - Utilizador\n"+
                "2 - Voluntario\n"+
                "3 - Empresa Transportadora\n"+
                "4 - Loja\n"+
                "\n0 - Voltar");
    }
}
