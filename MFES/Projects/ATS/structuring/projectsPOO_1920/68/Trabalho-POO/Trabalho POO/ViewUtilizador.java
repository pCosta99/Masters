import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class ViewUtilizador implements Serializable {

    public void menu(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                   Menu                                    |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        1. Fazer uma Encomenda                                             |");
        System.out.println("|        2. Ver entregas de um Voluntário ou uma Transportadora             |");
        System.out.println("|        0. Logout                                                          |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("     Digite a sua opção");
        System.out.print("\n\n");
    }


    public void fazerEncomenda_mostrarLojas(Collection<Loja> lojas ){
        System.out.println("________________________________________________________________________________________");
        System.out.println("|                                                                                       |");
        System.out.println("|                                    Fazer uma Encomenda                                |");
        System.out.println("|_______________________________________________________________________________________|");
        System.out.println();
        System.out.println("   -> Escolha uma das loja digitando o seu código");
        System.out.println();
        System.out.println();
        for (Loja l: lojas){
            String cod= l.getCodLoja();
            System.out.print("      Código: "+ cod);
            int nrEspacos=30-cod.length(); while(nrEspacos>0){System.out.print(" "); nrEspacos--;}
            System.out.println("      Nome: "+ l.getNomeLoja());
            System.out.println();
        }
        System.out.println("_________________________________________________________________________________________");
        System.out.print("\n\n");
    }

    public void FazerEncomenda_mostrarProduto(List<LinhaEncomenda> produtos){
        System.out.println("_____________________________________________________________________________________________________________");
        System.out.println("|                                                                                                            |");
        System.out.println("|                                               Fazer uma Encomenda                                          |");
        System.out.println("|____________________________________________________________________________________________________________|");
        System.out.println();
        System.out.println("   -> Escolha os produtos que quer encomendar, digitanto o codido (seguido de Enter), ");
        System.out.println("      e a seguir introduza a quantidade em Kgs (seguido de Enter)");
        System.out.println();
        System.out.println("   -> Quando tiver escolhido todos os produtos que deseja comprar digize 'q'");
        System.out.println();
        System.out.println();
        for (LinhaEncomenda p: produtos){
            System.out.print("      Código: "+ p.getCodProd());
            int nrEspaços = 9-p.getCodProd().length(); while(nrEspaços>0) {System.out.print(" "); nrEspaços--;}
            System.out.print(" Nome: "+ p.getDescricao());
            nrEspaços= 30-p.getDescricao().length(); while(nrEspaços>0) {System.out.print(" "); nrEspaços--;}
            System.out.print("  Preço por Kg: "+p.getPrecoUnitario());
            System.out.println();
        }
        System.out.println("____________________________________________________________________________________________________________");
        System.out.print("\n\n");
    }

    public void AceitarTransportadoraParaEncomenda(Double precoTransporte, String nomeTransportadora){
        BigDecimal bd =new BigDecimal(precoTransporte).setScale(2, RoundingMode.HALF_EVEN);
        precoTransporte = bd.doubleValue();

        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                 Aceitar Encomenda                         |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        Não existem Voluntários disponíveis.                               |");
        System.out.println("|        A sua Encomenda pode ser transportada pela Empresa                 |");
        System.out.println("|        "+nomeTransportadora+".");
        int nrEspacos= 66-nomeTransportadora.length(); while(nrEspacos>0){nrEspacos--; System.out.print(" ");} System.out.println("|");
        System.out.println("|        Custo do Transporte: " + precoTransporte);
        nrEspacos = 46-numero_digitos(precoTransporte); while(nrEspacos>0){nrEspacos--; System.out.print(" ");} System.out.println("|");
        System.out.println("|                                                                           |");
        System.out.println("|        Deseja aceitar o transporte da sua Encomenda                       |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("                   s: Sim                           n:Não                   ");
        System.out.print("\n\n");
    }

    public void tempoEspera(double tempo){
        BigDecimal bd =new BigDecimal(tempo).setScale(2, RoundingMode.HALF_EVEN);
        tempo = bd.doubleValue();
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                     Entrega                               |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println();
        System.out.println("     A sua encomenda vai demorar "+ tempo +" minutos.");
        System.out.print("\n\n");
    }


    public void classificar(String codTransporta){
        int nrEspacos = 35- codTransporta.length();
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                              Classificar Entrega                          |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.print("|       A sua encomenda foi entregue por " + codTransporta); while (nrEspacos>0){System.out.print(" "); nrEspacos--;} System.out.println("|");
        System.out.println("|                                                                           |");
        System.out.println("|       Se desejar avaliar o serviço de entega, digite um número entre      |");
        System.out.println("|       1 a 10. Caso contrário digite 'q'.                                  |");
        System.out.println("|___________________________________________________________________________|");
        System.out.print("\n\n");
    }


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // -> aceder à informação das entregas efectuadas num determinado período e por voluntárioou transportador;

    // -> Para opcao 2 falta imprimir extratos
    public void menuDeExtratos(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|        Extrato de  entregas de um Voluntário ou uma Transportadora        |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        1. Extrato de um Voluntario                                        |");
        System.out.println("|        2. Extrato de uma Transportadora                                   |");
        System.out.println("|        0. Voltar ao menu principal                                        |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("     Digite a sua opção");
        System.out.print("\n\n");
    }


    public void EscolheVoluntarioParaExtrato(Collection<Voluntario> voluntarios){
        System.out.println("_______________________________________________________________________________________________________");
        System.out.println("|                                                                                                     |");
        System.out.println("|                                Extrato de entregas de um Voluntário                                 |");
        System.out.println("|_____________________________________________________________________________________________________|");
        System.out.println();
        System.out.println("   -> Escolha um voluntário digitando o seu código");
        System.out.println();
        System.out.println();
        for (Voluntario v: voluntarios){
            String cod= v.getCodigo();
            System.out.println("      Código: "+ cod);
            int nrEspacos=50-cod.length(); while(nrEspacos>0){System.out.print(" "); nrEspacos--;}
            System.out.println("      Nome: "+ v.getNome());
            System.out.println();
        }
        System.out.println("_______________________________________________________________________________________________________");
        System.out.println(); System.out.println();
    }



    public void EscolheTransportadoraParaExtrato(Collection<Transportadora> transportadoras){
        System.out.println("_______________________________________________________________________________________________________");
        System.out.println("|                                                                                                     |");
        System.out.println("|                             Extrato de entregas de uma Transportadora                               |");
        System.out.println("|_____________________________________________________________________________________________________|");
        System.out.println();
        System.out.println("   -> Escolha uma transportadora digitando o seu código");
        System.out.println();
        System.out.println();
        for (Transportadora t: transportadoras){
            String cod= t.getCodigo();
            System.out.println("      Código: "+ cod);
            int nrEspacos=50-cod.length(); while(nrEspacos>0){System.out.print(" "); nrEspacos--;}
            System.out.println("      Nome: "+ t.getNome());
            System.out.println();
        }
        System.out.println("_______________________________________________________________________________________________________");
        System.out.println(); System.out.println();
    }

    public void IntervaloDeTempoParaExtratos(){
        System.out.println("______________________________________________________________________");
        System.out.println("|                                                                     |");
        System.out.println("|                         Extrato de Entregas                         |");
        System.out.println("|_____________________________________________________________________|");
        System.out.println("|                                                                     !");
        System.out.println("|        -> Digite a data de início do extrato                        |");
        System.out.println("|        -> Digite a data do fim do Extrato                           |");
        System.out.println("|                                                                     |");
        System.out.println("|  Info: As datas têm de ser escritas na seguinte ordem:              |");
        System.out.println("|       ->Ano (Enter)                                                 |");
        System.out.println("|       ->Mês (Enter)                                                 |");
        System.out.println("|       ->Dia (Enter)                                                 |");
        System.out.println("|       ->Hora (Enter)                                                |");
        System.out.println("|_____________________________________________________________________|");
        System.out.print("\n\n");
    }



    public void extrato(int total, List<Encomenda> encomendas){
        BigDecimal bd; double peso;
        System.out.println("\n\n\n");
        System.out.println("_____________________________________________________________________________");
        System.out.println("|                                                                            |");
        System.out.println("|                                Extrato de Entregas                         |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println("    Total de Encomendas: " + total);
        System.out.println("\n");
        for (Encomenda e: encomendas){
            int nrEspacos=8-e.getCodEnc().length();
            System.out.print("    Código de Encomenda: "+e.getCodEnc());
            while(nrEspacos<0){nrEspacos--;System.out.print(" ");}
            bd =new BigDecimal(e.pesoProdutos()).setScale(3, RoundingMode.HALF_EVEN);
            peso = bd.doubleValue();
            System.out.print("    Peso da Encomenda: " +peso);

            System.out.println();
        }
        System.out.println("\n\n\n");
    }

    public void PrintMensagem(String msg){
        System.out.println("\n" + msg);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private int numero_digitos(double numero) {
        String valueString = numero + "";
        int count = valueString.length();
        return count;
    }
}
