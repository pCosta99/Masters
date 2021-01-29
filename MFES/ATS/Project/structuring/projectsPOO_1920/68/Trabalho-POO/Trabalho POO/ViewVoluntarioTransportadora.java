import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

public class ViewVoluntarioTransportadora implements Serializable {


    public void menuVoluntario(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                   Menu                                    |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        1. Ver extrato de Encomendas                                       |");
        System.out.println("|        2. Ver classificações                                              |");
        System.out.println("|        3. Número total de encomendas entregues                            |");
        System.out.println("|        0. Logout                                                          |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("     Digite a sua opção");
        System.out.print("\n\n");
    }

    public void menuTransportadora(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                   Menu                                    |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        1. Ver extrato de Encomendas                                       |");
        System.out.println("|        2. Ver classificações                                              |");
        System.out.println("|        3. Número total de encomendas entregues                            |");
        System.out.println("|        4. Total faturado através da aplicação                             |");
        System.out.println("|        0. Logout                                                          |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("     Digite a sua opção");
        System.out.print("\n\n");
    }

////////////////////////////////////////////////Extrato//////////////////////////////////////////////////////////////////
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

    public void extratoVoluntario(int total, List<Encomenda> encomendas){
        BigDecimal bd; double peso;
        System.out.println("");
        System.out.println("_____________________________________________________________________________");
        System.out.println("|                                                                            |");
        System.out.println("|                              Extrato de Entregas                           |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println();
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


    public void extratoTransportadora(int total, List<Encomenda> encomendas, double faturado){
        BigDecimal bd; double peso;
        bd =new BigDecimal(faturado).setScale(4, RoundingMode.HALF_EVEN);
        faturado = bd.doubleValue();
        System.out.println("");
        System.out.println("___________________________________________________________________________________");
        System.out.println("|                                                                                  |");
        System.out.println("|                                Extrato de Entregas                               |");
        System.out.println("|__________________________________________________________________________________|");
        System.out.println();
        System.out.println("    Total de Encomendas: " + total+          "  Total Faturado: " + faturado);
        System.out.println("\n");
        for (Encomenda e: encomendas){
            int nrEspacos=13-e.getCodEnc().length();
            System.out.print("    Código de Encomenda: "+e.getCodEnc());
            while(nrEspacos<0){nrEspacos--;System.out.print(" ");}
            bd =new BigDecimal(e.pesoProdutos()).setScale(3, RoundingMode.HALF_EVEN);
            peso = bd.doubleValue();
            System.out.print("    Peso da Encomenda: " +peso);
            System.out.println();
        }
        System.out.println("\n\n\n");
    }

/////////////////////////////////////////////////////Classificacoes//////////////////////////////////////

    public void classificacao(int classificacao, int total){
        int nrEspaco1= 40-numero_digitos(total);
        int nrEspacos2=51-numero_digitos(classificacao);
        System.out.println("");
        System.out.println("_____________________________________________________________________________");
        System.out.println("|                                                                            |");
        System.out.println("|                               Classificação                                |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println("|                                                                            |");
        System.out.print("|    Número total de Classificações: " + total); while(nrEspaco1>0){System.out.print(" "); nrEspaco1--;} System.out.println("|");
        System.out.println("|                                                                            |");
        System.out.print("|    Classificação média: " + classificacao); while(nrEspacos2>0){System.out.print(" "); nrEspacos2--;} System.out.println("|");
        System.out.println("|                                                                            |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println("\n\n\n");
    }

////////////////////////////////////////////////Numero total de entregas////////////////////////////////////////

    public void totalDeEntregas(int total){
        int nrEspaco1= 46-numero_digitos(total);
        System.out.println("");
        System.out.println("_____________________________________________________________________________");
        System.out.println("|                                                                            |");
        System.out.println("|                            Total de entregas                               |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println("|                                                                            |");
        System.out.print("|    Número total de Entregas: " + total); while(nrEspaco1>0){System.out.print(" "); nrEspaco1--;} System.out.println("|");
        System.out.println("|                                                                            |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println("\n\n\n");
    }

    ////////////////////////////////////////////////Total faturado////////////////////////////////////////
//usar só na Transportadora
    public void totalFarurado(double total){

        BigDecimal bd =new BigDecimal(total).setScale(4, RoundingMode.HALF_EVEN);
        total = bd.doubleValue();
        int nrEspaco1= 50-numero_digitos(total);
        System.out.println("");
        System.out.println("_____________________________________________________________________________");
        System.out.println("|                                                                            |");
        System.out.println("|                               Total faturado                               |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println("|                                                                            |");
        System.out.print("|    Valor total faturado: " + total); while(nrEspaco1>0){System.out.print(" "); nrEspaco1--;} System.out.println("|");
        System.out.println("|                                                                            |");
        System.out.println("|____________________________________________________________________________|");
        System.out.println("\n\n\n");
    }

/////////////////////////////////////AUX////////////////////////////////////////////////////////////////////////
public void PrintMensagem(String msg){
    System.out.println("\n" + msg);
}

private int numero_digitos(int numero) {
    int count = 0;
    while(numero != 0) {
        numero /= 10;
        ++count;
    }
    if(count==0) count++;
    return count;
}

    private int numero_digitos(double numero) {
        String valueString = numero + "";
        int count = valueString.length();
        return count;
    }


}
