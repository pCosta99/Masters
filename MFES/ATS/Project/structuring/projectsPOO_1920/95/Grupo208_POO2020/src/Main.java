import Enums.Tipo;

/**
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Main {
    public static void main(String[] Args) {
        BaseDeDados bd     = new BaseDeDados("../Dados/logs.txt");
        View view          = new View();
        Menu menuPrincipal = new Menu(new String[]{"Login",
                                                   "Registo",
                                                   "Carregar dados de ficheiro",
                                                   "Guardar dados em ficheiro",
                                                   "Indicar o total faturado por uma empresa num determinado periodo",
                                                   "Top 10 utilizadores que mais utilizaram o sistema em termos de nº de encomendas",
                                                   "Top 10 empresas que mais utilizaram o sistema em termos de kms percorridos",
                                                   "Ver classificação de um Transportador"});
        MenuTipo menuTipo  = new MenuTipo(Tipo.allOptions());

        new Controller(bd,view,menuPrincipal,menuTipo).run();
    }
}