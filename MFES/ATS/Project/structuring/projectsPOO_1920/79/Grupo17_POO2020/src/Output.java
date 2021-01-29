import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Arrays;

public class Output
{

    public static void inicia(){
        System.out.println("\n ---------------------------------------");
        System.out.println("|                Traz Aqui!              |");
        System.out.println(" ---------------------------------------\n");
    }

    public static void registaEnt(){System.out.println("Pretende registar se como:\n");}

    public static void pretende() {System.out.println("Pretende: ");}

    public static void inserirCampos(){System.out.println("Insira os seu dados.");}

    public static void indicaNome(){System.out.print("Introduza o nome: ");}

    public static void indicaCoord(){System.out.print("Introduza as suas coordenadas: ");}

    public static void indicaEmail(){System.out.print("Introduza o email: ");}

    public static void indicaPass(){System.out.print("Introduza a palavra-passe: ");}

    public static void indicaRaio(){System.out.print("Introduza o raio: ");}

    public static void indicaMed(){System.out.print("Digite 's' se aceita o transporte de medicamentos e 'n' caso contrario: ");}

    public static void indicaNif(){System.out.print("Introduza o numero de identificação fiscal(nif): ");}

    public static void indicaTaxa(){System.out.print("Introduza o preço por kilometro: ");}

    public static void indicaCap(){System.out.print("Introduza a capacidade da transportadora: ");}

    public static void opcaoInvalida(){
        System.out.println("Opcao invalida");
    }

    public static void prodMed(){System.out.print("Se o produto for de teor médico digite 's'.Caso contrario digite 'n'.");}

    public static void credenciais(){System.out.println("Insira o seu email e palavra-passe");}

    public static void credenciaisValidos(){System.out.println("Credenciais validos");}

    public static void credenciaisInvalidos(){System.out.println("Credenciais invalidos. Por favor insira novamente.");}

    public static void lojasDisponiveis(){System.out.println("As lojas disponiveis sao:");}

    public static void digiteLoja(){System.out.println("Digite o codigo da loja que pretende");}

    public static void produtosDisponiveis(){System.out.println("O catalogo da loja que selecionou:");}

    public static void produtosAcomprar(){System.out.println("Selecione o produto(o seu codigo) que deseja comprar e a respetiva quantidade.Se pretende adicionar mais produtos digite m.Se ja terminou de adicionar ao carrinho digite q");}

    public static void criaProduto(){System.out.println("Digite a descricao do novo produto, o seu preço e peso");}

    public static void indicaDesc(){System.out.println("Descricao: ");}

    public static void indicaPreco(){System.out.println("Preco: ");}

    public static void indicaPeso(){System.out.println("Peso: ");}

    public static void transpDisponiveis(){System.out.println("As entidades disponiveis para fazer a sua encomenda sao:");}

    public static void selecionaTrans(){System.out.println("Digite a entidade(o seu codigo) que pretende que realize a sua entrega");}

    public static void removeProduto(){System.out.println("Insira o codigo de produto que deseja remover");}

    public static void catalogo(){System.out.println("O catalogo da sua loja:");}

    public static void sucessoEnc(){System.out.println("Encomenda feita. Transportador a caminho.");}

    public static void volDispo(){System.out.println("Se está disposto digite 'd'. Se não está disposto digite 'n'.");}

    public static void encomendaFeita(){System.out.println("Encomenda realizada.");}

    public static void encomendaTempos(){System.out.println("Introduza o codigo de encomenda para o qual deseja ver os tempos");}

    public static void introduzClass(){System.out.println("Introduza a classificacao(0-10) que pretende dar a este transportador");}

    public static void novoProd(){System.out.println("Novo produto: ");}

    public static void indicaCod(){System.out.print("Indique o codigo: ");}

    public static void indicaQuant(){System.out.print("Indique a quantidade: ");}

    public static void maisProd(){System.out.println("Se deseja adicionar mais produtos ao catalogo da loja digite 's'.Caso contario digite 'n'.");}

    public static void dataInicio(){System.out.println("Insira a data e hora de inicio");}

    public static void dataFim(){System.out.println("Insira a data e hora de fim");}

    public static void hora(){System.out.print("Hora: ");}

    public static void minutos(){System.out.print("Minutos: ");}

    public static void dia(){System.out.print("Dia: ");}

    public static void mes(){System.out.print("Mes: ");}

    public static void ano(){System.out.print("Ano: ");}

    public static void printException(String s){System.out.println("Codigo " + s + " inválido");}

    public static void entNaoExiste(){System.out.println("Entidade não existe");}

    public static void traNaoExiste(){System.out.println("Não existem transportadoras disponíveis de momento.");}

    public static void lojNaoExiste(){System.out.println("Não existem lojas disponíveis de momento.");}

    public static void encNaoExiste(){System.out.println("Encomenda não existe");}

    public static void classificacaoInvalida(){System.out.println("Classificacao inválida ou não existe serviço para classificar.");}

    public static void traSemEnc(){System.out.println("Nao ha encomendas feitas por este transportador.");}

    public static void codEncs(Utilizador u) throws ConjuntoVazioException {System.out.println(u.codEncs());}

    public static void excCodInv(String s){System.out.println("O codigo " + s + " e invalido.Se quiser adicionar um novo produto digite 'm'.Caso contrario digite 'q'.");}

    public static void inserirCodEnc(){System.out.println("Insira o codigo de encomenda:");}

    public static void naoHaEnc(){System.out.println("Não existem mais encomendas para fazer.");}

    public static void encFeita(Encomenda enc){ System.out.println("Encomenda feita: " + enc.getCodEncomenda());}

    public static void imprimeCoord(SistemaGestao sg, Encomenda enc){ System.out.println(sg.imprimeCoord(enc.getDestinatario(), enc.getVendedor())); }

    public static void codEncs(SistemaGestao sg, Transportadora t) throws ConjuntoVazioException { System.out.println(sg.codEncs(t.getCodigo()));}

    public static void tempos(SistemaGestao sg, Transportadora t, String codEnc) throws CodigoInvalidoException { System.out.println(sg.getTempos(t.getCodigo(), codEnc));}

    public static void carrinho(Utilizador u){System.out.println(u.getCarrinho());}

    public static void lojas(SistemaGestao sg) throws ConjuntoVazioException {System.out.println(sg.nomesLojas());}

    public static void registos(SistemaGestao sg, String codigo) throws ConjuntoVazioException {System.out.println(sg.getUtilizadorSistema(codigo).getRegistos());}

    public static void codEncs(SistemaGestao sg, Voluntario v) throws ConjuntoVazioException {System.out.println(sg.codEncs(v.getCodigo()));}

    public static void tempos(SistemaGestao sg, Voluntario v, String codEnc) throws CodigoInvalidoException {System.out.println(sg.getTempos(v.getCodigo(), codEnc));}

    public static void utilizadorSistema(SistemaGestao sg, Voluntario v) throws ConjuntoVazioException {System.out.println(sg.getUtilizadorSistema(v.getCodigo()).getRegistos());}

    public static void parametros(SistemaGestao sg, Voluntario v){System.out.println(sg.getParametros(v.getCodigo()));}

    public static void utilizadorSistema(SistemaGestao sg, Transportadora t) throws ConjuntoVazioException {System.out.println(sg.getUtilizadorSistema(t.getCodigo()).getRegistos());}

    public static void parametros(SistemaGestao sg, Transportadora t){System.out.println(sg.getParametros(t.getCodigo()));}

    public static void faturacaoTransp(SistemaGestao sg, LocalDateTime inicio, LocalDateTime fim, Transportadora t){System.out.println(sg.faturacaoTransp(inicio, fim, t.getCodigo()));}

    public static void pesoPreco(){System.out.println("Preço ou peso inválidos.");}

    public static void encomendas(Loja l){System.out.println(l.getEncomendas());}

    public static void utilizadorSistema(SistemaGestao sg, String codigo) throws ConjuntoVazioException {System.out.println(sg.getUtilizadorSistema(codigo).getRegistos());}

    public static void catSemProd(){System.out.println("Loja sem produtos no seu catalogo.");}

    public static void sg(SistemaGestao sg){
        System.out.println("Entidades:");
        System.out.println(sg.toStringEntidades());
        System.out.println("Encomendas:");
        System.out.println(sg.toStringEncomendas());
    }

    public static void utiMaisUsados(SistemaGestao sg) throws ConjuntoVazioException {System.out.println(sg.utilizadoresMaisUsados());}

    public static void semUtiException(){System.out.println("Nao existem utlizadores no sistema.");}

    public static void traMaisUsadas(SistemaGestao sg) throws ConjuntoVazioException {System.out.println(sg.transportadorasMaisUsados());}

    public static void semTraException(){System.out.println("Nao existem transportadoras no sistema.");}

    public static void insiraPath(){System.out.println("Insira o path do ficheiro logs: ");}

    public static void ficheiroException(){System.out.println("Erro ao ler ficheiro");}

    public static void guardarException(){System.out.println("Erro ao gravar ficheiro");}

    public static void carregado(){System.out.println("Ficheiro carregado com sucesso");}

    public static void guardado(){System.out.println("Ficheiro guardado com sucesso");}

    public static void dataInvalida(){System.out.println("Data inválida");}

    public static void catalogo(SistemaGestao sg, Loja l) throws ConjuntoVazioException {
        if(l.nmrProdCat()<=0) throw new ConjuntoVazioException();
        else {
            for (LinhaEnc le : l.getCatalogo().values()) {
                System.out.println(le.imprime());
            }
        }
    }

    public static void filtraTransportadoras(SistemaGestao sg,Encomenda en,Utilizador u,Loja l){
        for(Entidade e : sg.filtraTransportadoras(en,u,l)){
            EntidadeEntrega ee = (EntidadeEntrega) e;
            System.out.println(ee.imprime(en,u.getXGPS(),u.getYGPS(),l.getXGPS(),l.getYGPS()));
        }
    }

    public static void tentarNovo(){
        System.out.println("Deseja tentar de novo? Se sim, digite 's', caso contrario digite 'n'");
    }

    public static void clearScreen() {
        for (int i = 0; i < 50; ++i) System.out.println();
    }

    public static void voltar(){
        System.out.println("-------------------------");
        System.out.println("Pressione 'V' para voltar.");
        System.out.println("-------------------------");
    }

    public static Menu menuU(){
        return new Menu(Arrays.asList("Ver carrinho.",
                "Fazer uma encomenda.",
                "Classificar a entidade que transportou a sua encomenda.",
                "Ver o seu historico de encomendas.",
                "Escolher uma entidade de transporte para uma encomenda que ainda não tem transportador associado"));
    }

    public static Menu menuV(){
        return new Menu(Arrays.asList("Alterar a sua disposicao para recolher encomendas.",
                "Realizar uma encomenda.",
                "Visualizar o inicio, fim e o tempo de entrega de uma encomenda.",
                "Ver o seu historico de encomendas.",
                "Visualizar a classificacao, numero de kilometros feitos e quantos servicos tem.",
                "Alterar o transporte de encomendas medicas."));
    }

    public static Menu menuT(){
        return new Menu(Arrays.asList("Realizar uma entrega.",
                "Visualizar o inicio, fim e o tempo de entrega de uma encomenda.",
                "Ver o seu historico de encomendas.",
                "Visualizar a classificacao, numero de kilometros feitos e quantos servicos tem.",
                "Alterar o transporte de encomendas medicas.",
                "Visualizar a faturaçao da empresa num determinado periodo de tempo."));
    }


    public static Menu menuL(){
        return new Menu(Arrays.asList("Registar um novo produto da sua loja.",
                "Visualizar as encomendas feitas pelos utilizadores.",
                "Ver o seu historico de encomendas.",
                "Remover um produto do catalogo da loja."));
    }


    public static Menu menuPrincipal() {
        return new Menu(Arrays.asList("Registar uma Entidade.",
                "Fazer login.",
                "Ver entidades atual do sistema.",
                "Listar os 10 Utilizadores(os seus codigos) que mais utilizam o sistema.",
                "Listar as 10 Transportadoras(os seus codigos) que mais utilizam o sistema.",
                "Importar dados do ficheiro logs.",
                "Gravar dados em ficheiro(em binario).",
                "Ler dados em ficheiro(em binario)."));
    }

    public static Menu menuEntidade(){
        return new Menu(Arrays.asList("Utilizador","Voluntario","Loja","Transportadora"));
    }


    public static Menu menuEncV() {
        return new Menu(Arrays.asList("Finalizar uma encomenda.","Visualizar coordenadas da loja e destinatario da encomenda."));
    }

    public static Menu menuEncT() {
        return new Menu(Arrays.asList("Finalizar uma encomenda.","Visualizar coordenadas da loja e destinatario da proxima encomenda a fazer."));
    }

}