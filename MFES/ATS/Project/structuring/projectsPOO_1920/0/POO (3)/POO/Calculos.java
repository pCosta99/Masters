import java.time.LocalDate;

public class Calculos {
    
    public double distancia(double xi, double yi, double xf, double yf) {
        double x = xf - xi;
        double y = yf - yi;
        double calc = x * x + y * y;
        return Math.round(Math.sqrt(calc) * 10000.0) / 10000.0;
    }
    
    
    public double custo(double distancia, double peso, double precoKm, double precoKg, double taxa) {
        double custoPeso = peso * precoKg;
        double custoDist = distancia * precoKm;
        double valorTaxa = (custoPeso + custoDist) * taxa;
        return Math.round((custoPeso + custoDist + valorTaxa) * 10000.0) / 10000.0;
    }
    
    
    public boolean isBetween(LocalDate dataAtual, LocalDate dataInicial, LocalDate dataFinal){
        boolean res = dataAtual.isAfter(dataInicial);
        if(res){
            res = dataAtual.isBefore(dataFinal);
        }
        return res;
    }
    
    
}
