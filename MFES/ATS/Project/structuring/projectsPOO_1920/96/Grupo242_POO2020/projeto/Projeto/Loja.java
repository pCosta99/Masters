package Projeto;

//Nas lojas onde não existe informação de fila de esperaassume-se
// que os voluntários e as empresas transportadoras assumem o risco
// de poder passar maistempo a ser atendidos e tal, no caso das empresas,
// não leva a aumento da tarifa de transporte.

import java.util.ArrayList;

public class Loja extends Utilizador{
    ArrayList<LinhaEncomendas> enc;

    public Loja(String email, String nome, String pass, Posicao pos, ArrayList<LinhaEncomendas> enc) {
        super(email,nome,pass,pos);
        this.enc = enc;
    }

    public Loja(Utilizador user, ArrayList<LinhaEncomendas> enc) {
        super(user);
        this.enc = enc;
    }

    public Loja(Posicao pos, ArrayList<LinhaEncomendas> enc) {
        super(pos);
        this.enc = enc;
    }

    public Posicao getPos(){
        return super.getPos();
    }
    public Loja(Loja arg){
        super(arg);
    }

    public void addEncomenda(LinhaEncomendas enc){

        this.enc.add(enc);
    }


    @Override
    public String getEmail() {
        return super.getEmail();
    }

    public Loja clone(Loja arg){
        return new Loja(arg);
    }

    @Override
    public String toString() {
        return "Loja{" +
                "code=" + getEmail() + '\'' +
                "nome" + getNome() + '\'' +
                "pos" + getPos() + '\'' +
                "LinhaEncomendas=" + enc + '\'' +
                '}';
    }

//public double static tempoRecolha(){} Essa estimativa tem em consideração
    // o tempo que se percorredo sítio onde está quem recolhe a encomenda
    // até à loja (é necessário saber a que velocidade sedesloca) e o tempo
    // de espera em função da fila de espera da loja.

    //public double static tempoMedio(){} = tempoMedio
}
