package View;

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

/** Classe que fornece métodos para o formato da impressão na vista. */
public class PrintFormat {

    /**
     * Devolve um valor monetário formatado para a região portuguesa.
     * 
     * @param n Valor a ser formatado.
     * @return  String com o valor formatado.
     */
    public static String currencyFormat(BigDecimal n) {
        return NumberFormat.getInstance(new Locale("pt", "PT")).format(n) + " €";
    }
    
    
    /**
     * Calcula o número de dígitos num dado inteiro.
     * 
     * @param n Inteiro a ser analisado.
     * @return Número de dígitos.
     */
    public static int digitsCount(int n) {
        int count = 0;

        if (n == 0)
            count = 1;
        else while (n != 0) {
            n /= 10;
            count++;
        }
        
        return count;
    }

    /**
     * Formata um LocalDateTime
     * 
     * @param ldt Data/hora a ser formatada.
     * @return  String com o valor formatado.
     */
    public static String dateFormat(LocalDateTime ldt) {
        if (ldt == null)
            return "N/A";
        return ldt.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm"));
    }



}
