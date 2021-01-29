import java.util.List;
import java.util.Map;

/**
 * Classe TrazAquiView que contém todos os métodos responsáveis por realizar prints do programa.
 */
public class TrazAquiView implements ITrazAquiView {

    /**
     * Método que imprime Encomendas, nomeadamente, o código da loja das encomendas, o código das encomendas e o custo das encomendas.
     * @param codLoja Lista com todos os códigos das lojas associadas ás encomendas.
     * @param codEnc Lista com todos os códigos das encomendas.
     * @param preco Lista com todos os custos associados ás encomendas.
     * @param medicas Lista das encomendas médicas.
     */
    public void imprimeEnc2(List<String> codLoja,List<String> codEnc,List<Double> preco,List<String> medicas){
        System.out.print("\n");
        for(int i = 0;i<codEnc.size();i++)
            if(!medicas.contains(codEnc.get(i)))
            System.out.println("Encomenda:Loja:"+codLoja.get(i)+" ,Codigo:"+codEnc.get(i)+" ,Custo:" +String.format("%.2f",preco.get(i))+"€");
            else{
                System.out.println("Encomenda:Loja:"+codLoja.get(i)+" ,Codigo:"+codEnc.get(i)+" ,Custo:" +String.format("%.2f",preco.get(i))+"€, Encomenda Médica");
            }
    }

    /**
     * Método que imprime o código e o nome do Voluntário a que a encomenda foi entregue.
     * @param cod Código do Voluntário ao qual a encomenda foi entregue.
     * @param nome Nome do Voluntário ao qual a encomenda foi entregue.
     */
    public void imprimeVoluntario(String cod,String nome){
        System.out.println("\nA sua encomenda foi entregue ao voluntario:"+cod+", "+nome);
    }

    /**
     * Método que imprime uma Transportadora e os dados da entrega caso ela fosse realizada pela Transportadora.
     * @param cod Código do Transportadora.
     * @param nome Nome da Transportadora.
     * @param dist Distância que a Transportadora terá de percorrer para realizar a entrega.
     * @param preco Preco da entrega.
     * @param tempo Tempo que a entrega irá demorar.
     * @param clas Classificação da Transportadora.
     */
    public void imprimeTransp(String cod,String nome,String dist,double preco,double tempo,double clas){
        System.out.println("Transportadora:"+cod+", "+nome);
        System.out.println("Distância: "+dist+ "kms, "+(int)tempo/60+ " horas e " +(int)tempo%60+" minutos.");
        System.out.println("Preço: "+String.format("%.2f",preco)+"€");
        System.out.println("Classificação: "+String.format("%.2f",clas));
        System.out.println("------------------------------------------------------");

    }

    /**
     * Método que imprime as Encomendas não aceites.
     * @param codEnc Lista com todos os códigos de Encomenda.
     * @param aceites Lista com todos os códigos das Encmendas Aceites.
     */
    public void imprimeEncs(List<String> codEnc,List<String> aceites){
        for (int i = 0; i < codEnc.size(); i++) {
            if(!aceites.contains(codEnc.get(i)))
            System.out.println((i+1)+" -> "+codEnc.get(i));
            else System.out.println((i+1)+" -> "+codEnc.get(i) +" [Classificar]");
        }
        System.out.println(codEnc.size()+1 + " -> Sair");
        System.out.println("\nEscolha uma opção:");
    }

    /**
     * Método que imprime o Catálogo de produtos de uma loja.
     * @param l Lista de produtos do Catálogo.
     */
    public void imprimeProds(List<Map.Entry<String, Map.Entry<String,Double>>> l ){
        System.out.println("\nProdutos disponiveis na loja:\n");
        for (Map.Entry<String, Map.Entry<String, Double>> e : l)
            System.out.println(e.getKey() + " -> " + e.getValue().getKey() + " ," + String.format("%.2f",e.getValue().getValue()));
        System.out.println("q -> Acabar encomenda");
    }

    /**
     * Método que imprime todas as lojas.
     * @param l Lista contendo o Código e o respetivo nome das lojas.
     */
    public void imprimeLojas(List<Map.Entry<String,String>> l ){
        System.out.println("\nLojas disponiveis");
        for(int i = 0;i<l.size();i++)
            System.out.println((i+1) +" -> " +l.get(i).getKey()+" ,"+l.get(i).getValue());
    }

    /**
     * Método que imprime os 10 melhores Clientes.
     * @param res Lista contendo o código dos Clientes e o número de encomendas realizadas pelo mesmo, ordenada decrescentemente pelo número de encomendas.
     */
    public void imprimeTopCli(List<Map.Entry<String,Integer>> res){
        for(int i=0 ; i<res.size() && i<10; i++){
            System.out.println(i+1 + "º - " + res.get(i).getKey() + " - " + res.get(i).getValue() + " encomendas recebidas.");
        }
    }
    /**
     * Método que imprime as 10 melhores Empresas transportadoras.
     * @param res Lista contendo o Nome das Empresas Transportadoras e o número de kilomentros percorridos pela mesma, ordenada decrescentemente pelo número de kilometros percorrido.
     */
    public void imprimeTopTr(List<Map.Entry<String,Double>> res){
        for(int i=0 ; i<res.size() && i<10; i++){
            System.out.println(i+1 + "º - " + res.get(i).getKey() + " - " + String.format("%.2f",res.get(i).getValue()) + " Kilometros.");
        }
    }
    /**
     * Método que imprime vários menus.
     * @param print Menu a imprimir.
     * @a String caso desejemos imprimir informação adicional.
     * */
    public void askForPrint(Print print,String a){
        switch (print){
            case Inicial:
                System.out.println("\n");
                System.out.println("----------------------------------");
                System.out.println("------------ TrazAqui ------------");
                System.out.println("----------------------------------");
                System.out.println("Escolha uma opção:");
                System.out.println("1 - Fazer login");
                System.out.println("2 - Criar conta");
                System.out.println("3 - Guardar estado do programa");
                System.out.println("4 - Carregar estado do progama");
                System.out.println("5 - Top 10 melhores clientes");
                System.out.println("6 - Top 10 melhores empresas transportadoras");
                System.out.println("7 - Sair do programa");
                break;
            case SignUp:
                System.out.println("\n");
                System.out.println("----------------------------------");
                System.out.println("------------- Sign Up ------------");
                System.out.println("----------------------------------");
                System.out.println("Escolha uma opção:");
                System.out.println("1 - Utilizador");
                System.out.println("2 - Voluntario");
                System.out.println("3 - Transportadora");
                System.out.println("4 - Loja");
                System.out.println("5 - Go back");
                break;
            case NotEncomenda:
                System.out.println("\nCarrinho Vazio");
                break;
            case Invalid:
                System.out.println("\nInput Invalido");
                break;
            case Nome:
                System.out.println("\nInsira o seu nome:");
                break;
            case NomeUt:
                System.out.println("\nInserir nome da conta:");
                break;
            case Coordenada:
                System.out.println("\nInserir coordenada "+a+":");
                break;
            case Cena:
                System.out.println("\nInserir "+a+":");
                break;
            case Mail:
                System.out.println("\nInserir email:");
                break;
            case Pass:
                System.out.println("\nInserir password:");
                break;
            case BadLogin:
                System.out.println("\nEmail ou password errados");
                break;
            case Utilizador:
                System.out.println("\n---------------------------------------");
                System.out.println("\t\tBem vindo utilizador "+a+"\n");
                System.out.println("Escolha uma opção:\n");
                System.out.println("1 - Fazer nova encomenda.");
                System.out.println("2 - Solicitar encomenda.");
                System.out.println("3 - Informacao encomendas.");
                System.out.println("4 - Logout");
                break;
            case Voluntario:
                System.out.println("\n---------------------------------------");
                System.out.println("\t\tBem vindo voluntario "+a+"\n");
                System.out.println("Escolha uma opção:\n");
                System.out.println("1 - Ver encomendas pendentes.");
                System.out.println("2 - Ver Historico de encomendas entregues.");
                System.out.println("3 - Ver classificação.");
                System.out.println("4 - Fazer entregas médicas.");
                System.out.println("5 - Logout");
                break;
            case Transportadora:
                System.out.println("\n-----------------------------------------");
                System.out.println("\t\tBem vindo Transportador "+a+"\n");
                System.out.println("Escolha uma opção:\n");
                System.out.println("1 - Ver Historico de encomendas entregues.");
                System.out.println("2 - Ver classificação.");
                System.out.println("3 - Ver total faturado.");
                System.out.println("4 - Fazer entregas médicas.");
                System.out.println("5 - Logout");
                break;
            case Loja:
                System.out.println("\n-------------------------------------");
                System.out.println("\n\t\tBem vindo Loja "+a+"\n");
                System.out.println("Escolha uma opção:\n");
                System.out.println("1 - Aceitar Encomendas.");
                System.out.println("2 - Ver Historico de encomendas.");
                System.out.println("3 - Acrescentar Produtos ao Catálogo");
                System.out.println("4 - Ver Catálogo");
                System.out.println("5 - Logout");
                break;
            case Classificacao:
                System.out.println("\nA sua classificação atual é de "+a+" estrelas.");
                break;
            case TotalFaturado:
                System.out.println("\nA sua faturação atual é de "+ a + " euros.");
                break;
            case Produto:
                System.out.println("\nInserir Produto:");
                break;
            case Quantidade:
                System.out.println("\nInserir quantidade:");
                break;
            case Carrinho:
                System.out.println("\nProduto "+a+" inserido no carrinho.");
                break;
            case Success:
                System.out.println("\n"+a+ " criada com sucesso");
                break;
            case NotEncomendas:
                System.out.println("\nNão tem encomendas pendentes");
                break;
            case TransYn:
                System.out.println("\nAceitar este transportador? [Y/n]");
                break;
            case Classificar:
                System.out.println("\nComo classifica este "+a+" de 0 a 5");
                break;
            case Erro:
                System.out.println(a);
                break;
            case Data:
                System.out.println("\nIntervalo de tempo para procurar as encomendas [DD/MM/AAAA]");
                break;
            case DataOp:
                System.out.println("\nEscolha uma opção:");
                System.out.println("1 - Voluntarios");
                System.out.println("2 - Transportadoras");
                break;
            case HistoricoVazio:
                System.out.println("\nNão foram efetuadas encomendas entre as datas dadas.");
                break;
            case AddProduto:
                System.out.println("\nInsira o código, o nome e preço do produto.");
                break;
            case EncomendaMedica:
                System.out.println("\nEsta encomenda é médica?");
                System.out.println("1 - Sim");
                System.out.println("2 - Não");
                break;
            case VMedico:
                System.out.println("\nPretende entregar encomendas médicas?");
                System.out.println("1 - Sim");
                System.out.println("2 - Nao");
                break;
            case NTrans:
                System.out.println("\nNão tem transportes disponiveis.");
                break;
            case Quit:
                System.out.println(a + " -> Sair.");
                break;
            case Ficheiro:
                System.out.println("Inserir nome do ficheiro para " + a + " [Enter para usar nome default]");
                break;

        }
    }
}
