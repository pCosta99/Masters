/**
 * Write a description of class pedido here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

import java.util.ArrayList;
public class EncomendaAceite extends Encomenda
{
    public EncomendaAceite(){
        super();
    } 
    public EncomendaAceite(String cod,String cod1, String cod2, double p, ArrayList<Produto> produtos, ArrayList<Entregas> entregas, ArrayList<Loja>loj, int est,String t){
        super(cod,cod1, cod2, p,produtos,entregas,loj,est, t);
    }
    public EncomendaAceite(EncomendaAceite e){
        super(e);
    }
}